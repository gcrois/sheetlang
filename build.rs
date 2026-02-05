use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    // Get the current date and time
    let output = Command::new("date")
        .env("TZ", "America/New_York")
        .arg("+%Y-%m-%d %H:%M:%S %Z")
        .output()
        .expect("Failed to execute date command");

    let datetime = String::from_utf8_lossy(&output.stdout);
    let datetime = datetime.trim();

    // Split into date, time, and timezone offset
    let parts: Vec<&str> = datetime.split_whitespace().collect();
    if parts.len() >= 2 {
        println!("cargo:rustc-env=BUILD_DATE={}", parts[0]);
        if parts.len() >= 3 {
            println!("cargo:rustc-env=BUILD_TIME={} {}", parts[1], parts[2]);
        } else {
            println!("cargo:rustc-env=BUILD_TIME={}", parts[1]);
        }
    } else {
        println!("cargo:rustc-env=BUILD_DATE=unknown");
        println!("cargo:rustc-env=BUILD_TIME=unknown");
    }

    let git_branch = git_output(&["rev-parse", "--abbrev-ref", "HEAD"])
        .filter(|s| s != "HEAD")
        .unwrap_or_else(|| "dev".to_string());
    let git_hash = git_output(&["rev-parse", "--short", "HEAD"])
        .unwrap_or_else(|| "dev".to_string());
    let git_dirty = git_output(&["status", "--porcelain"])
        .map(|s| !s.is_empty())
        .unwrap_or(false);
    let git_remote = git_output(&["config", "--get", "remote.origin.url"]);
    let git_commit_url = if git_hash == "dev" {
        "".to_string()
    } else {
        git_remote
            .as_deref()
            .and_then(normalize_git_remote)
            .map(|base| format!("{}/commit/{}", base, git_hash))
            .unwrap_or_else(|| "".to_string())
    };
    let git_branch_url = if git_branch == "dev" {
        "".to_string()
    } else {
        git_remote
            .as_deref()
            .and_then(normalize_git_remote)
            .map(|base| format!("{}/tree/{}", base, url_encode_component(&git_branch)))
            .unwrap_or_else(|| "".to_string())
    };

    println!("cargo:rustc-env=BUILD_GIT_BRANCH={}", git_branch);
    println!("cargo:rustc-env=BUILD_GIT_HASH={}", git_hash);
    println!("cargo:rustc-env=BUILD_GIT_COMMIT_URL={}", git_commit_url);
    println!("cargo:rustc-env=BUILD_GIT_BRANCH_URL={}", git_branch_url);
    println!(
        "cargo:rustc-env=BUILD_GIT_DIRTY={}",
        if git_dirty { "1" } else { "0" }
    );

    // Rebuild when git state changes (best-effort).
    if let Ok(head) = fs::read_to_string(".git/HEAD") {
        println!("cargo:rerun-if-changed=.git/HEAD");
        if let Some(ref_path) = head.strip_prefix("ref: ").map(|s| s.trim()) {
            println!("cargo:rerun-if-changed=.git/{}", ref_path);
        }
    }
    println!("cargo:rerun-if-changed=.git/index");
    println!("cargo:rerun-if-changed=.git/packed-refs");

    // Generate demo scripts code
    generate_demo_scripts();
}

fn normalize_git_remote(raw: &str) -> Option<String> {
    let raw = raw.trim();
    if raw.is_empty() {
        return None;
    }

    if let Some(rest) = raw.strip_prefix("git@") {
        let mut parts = rest.splitn(2, ':');
        let host = parts.next()?;
        let path = parts.next()?;
        return Some(format!("https://{}/{}", host, strip_git_suffix(path)));
    }

    if let Some(rest) = raw.strip_prefix("ssh://") {
        let rest = rest.strip_prefix("git@").unwrap_or(rest);
        let mut parts = rest.splitn(2, '/');
        let host = parts.next()?;
        let path = parts.next()?;
        return Some(format!("https://{}/{}", host, strip_git_suffix(path)));
    }

    if raw.starts_with("https://") || raw.starts_with("http://") {
        let trimmed = strip_git_suffix(raw).trim_end_matches('/');
        return Some(trimmed.to_string());
    }

    None
}

fn strip_git_suffix(value: &str) -> &str {
    value.trim_end_matches(".git")
}

fn url_encode_component(value: &str) -> String {
    let mut out = String::new();
    for b in value.as_bytes() {
        let ch = *b as char;
        if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' || ch == '.' || ch == '~' {
            out.push(ch);
        } else {
            out.push_str(&format!("%{:02X}", b));
        }
    }
    out
}

fn git_output(args: &[&str]) -> Option<String> {
    let output = Command::new("git").args(args).output().ok()?;
    if !output.status.success() {
        return None;
    }

    let value = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if value.is_empty() {
        None
    } else {
        Some(value)
    }
}

fn generate_demo_scripts() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("demo_scripts.rs");
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    // Scan tests/scripts directory for .sheet files
    let scripts_dir = Path::new(&manifest_dir).join("tests/scripts");
    let mut entries: Vec<_> = fs::read_dir(&scripts_dir)
        .expect("Failed to read tests/scripts directory")
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.path()
                .extension()
                .and_then(|s| s.to_str())
                .map(|s| s == "sheet")
                .unwrap_or(false)
        })
        .collect();

    // Sort entries by filename
    entries.sort_by_key(|e| e.file_name());

    // Generate code
    let mut code = String::new();
    code.push_str("// Auto-generated by build.rs\n\n");
    code.push_str("pub fn get_demo_script(n: u8) -> Option<&'static str> {\n");
    code.push_str("    match n {\n");

    for (index, entry) in entries.iter().enumerate() {
        let filename = entry.file_name();
        let filename_str = filename.to_str().unwrap();
        let path = entry.path();
        let path_str = path.to_str().unwrap();

        code.push_str(&format!(
            "        {} => Some(include_str!(\"{}\")), // {}\n",
            index + 1,
            path_str,
            filename_str
        ));
    }

    code.push_str("        _ => None,\n");
    code.push_str("    }\n");
    code.push_str("}\n\n");

    // Generate function to get demo count
    code.push_str(&format!(
        "pub fn get_demo_count() -> u8 {{\n    {}\n}}\n",
        entries.len()
    ));

    fs::write(&dest_path, code).unwrap();

    // Tell cargo to rerun if scripts directory changes
    println!("cargo:rerun-if-changed=tests/scripts");
}
