terraform {
  required_providers {
    cloudflare = {
      source  = "cloudflare/cloudflare"
      version = "~> 4.0"
    }
    github = {
      source  = "integrations/github"
      version = "~> 6.0"
    }
  }
}

variable "root_domain" {
  description = "The root domain (e.g. regory.dev)"
  type        = string
}

variable "subdomain" {
  description = "The subdomain prefix (e.g. cancan). Use '@' for root."
  type        = string
  default     = "cancan"
}

variable "cloudflare_api_token" {
  type      = string
  sensitive = true
}

variable "account_id" { type = string }
variable "zone_id" { type = string }
variable "project_name" { type = string }

variable "github_token" {
  description = "GitHub PAT with repo scope"
  type        = string
  sensitive   = true
}

variable "owner" {
  description = "GitHub username or org (e.g. gcrois)"
  type        = string
}

variable "repo_name" {
  description = "GitHub repository name (e.g. gregory.croisdale)"
  type        = string
}

variable "build_dir" {
  description = "Directory containing the built site to deploy"
  type        = string
  default     = "./dist"
}

# --- Providers ---
provider "cloudflare" {
  api_token = var.cloudflare_api_token
}

provider "github" {
  token = var.github_token
  owner = var.owner
}

# --- Cloudflare Resources ---
resource "cloudflare_pages_project" "static_site" {
  account_id        = var.account_id
  name              = var.project_name
  production_branch = "main"
}

resource "cloudflare_pages_domain" "custom_domain" {
  account_id   = var.account_id
  project_name = cloudflare_pages_project.static_site.name
  domain       = var.subdomain == "@" ? var.root_domain : "${var.subdomain}.${var.root_domain}"
}

resource "cloudflare_record" "site_dns" {
  zone_id = var.zone_id
  name    = var.subdomain
  type    = "CNAME"
  content = cloudflare_pages_project.static_site.subdomain
  proxied = true
}

resource "github_actions_secret" "cf_api_token" {
  repository      = var.repo_name
  secret_name     = "CLOUDFLARE_API_TOKEN"
  plaintext_value = var.cloudflare_api_token
}

resource "github_actions_secret" "cf_account_id" {
  repository      = var.repo_name
  secret_name     = "CLOUDFLARE_ACCOUNT_ID"
  plaintext_value = var.account_id
}

resource "github_actions_variable" "cf_project_name" {
  repository    = var.repo_name
  variable_name = "CLOUDFLARE_PROJECT_NAME"
  value         = var.project_name
}

resource "github_actions_variable" "cf_output_dir" {
  repository    = var.repo_name
  variable_name = "CLOUDFLARE_OUTPUT_DIR"
  value         = var.build_dir
}

output "full_url" {
  value = "https://${cloudflare_pages_domain.custom_domain.domain}"
}

output "deployment_command" {
  value = "npx wrangler pages deploy ${var.build_dir} --project-name=${var.project_name}"
}