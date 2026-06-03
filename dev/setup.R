# GitHub actions
# usethis::use_github_action()

# Add air GitHub actions
# usethis::use_github_action(url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-check.yaml")
# usethis::use_github_action(url = "https://github.com/etiennebacher/setup-jarl/blob/main/examples/jarl-check.yml")


# Snapshot
renv::status()
# renv::update(lock = TRUE)
renv::snapshot(type = "explicit", dev = TRUE)

spelling::update_wordlist()

# Bump versions
usethis::use_version("dev")

rsconnect::writeManifest()
