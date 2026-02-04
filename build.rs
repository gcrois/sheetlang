use std::process::Command;

fn main() {
    // Get the current date and time
    let output = Command::new("date")
        .arg("+%Y-%m-%d %H:%M:%S")
        .output()
        .expect("Failed to execute date command");

    let datetime = String::from_utf8_lossy(&output.stdout);
    let datetime = datetime.trim();

    // Split into date and time
    let parts: Vec<&str> = datetime.split_whitespace().collect();
    if parts.len() == 2 {
        println!("cargo:rustc-env=BUILD_DATE={}", parts[0]);
        println!("cargo:rustc-env=BUILD_TIME={}", parts[1]);
    } else {
        println!("cargo:rustc-env=BUILD_DATE=unknown");
        println!("cargo:rustc-env=BUILD_TIME=unknown");
    }
}
