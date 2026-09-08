---
name: docker-deploy
description: Build, tag, and deploy Neuromics Explorer's Shiny frontend and FastAPI backend images with Docker Compose on the private server. Use when the user wants to deploy, redeploy, ship, roll out, build images, bump a version tag, or bring the dev/prod stack up or down. Covers the standard "pull → build → tag → compose up" workflow and the project-name isolation that keeps other apps on the server alive.
---

# Bring up Neuromics Explorer stack

Source of truth is `docker-compose*.yml` + `Dockerfile`. If the files have changed and conflicts with this or other skills, trust them and flag the mismatch to the user, detailing the differences and a single, simple suggestion where appropriate.

**Pre-condition** the user must be connected to the server to invoke this skill instead of running it locally. Do not ty to connect or check the server, the skill should be invoked only after the user has already done so. If not, ask the user to connect first while providing the user the ssh command, then re-run skill.

## Connect with ssh

```bash
ssh dammy@128.40.163.137    # Abebe, not run just showing command used to prompt user to connect first
```

# Deploying Neuromics Explorer with Docker

The app ships as two images built from a single multi-stage `Dockerfile`:

| Target (`--target`) | Image | Port (in container) |
|---|---|---|
| `shiny-frontend` | `shesanislandukdri/neuromics_explorer_shiny` | 4848 |
| `fastapi-backend` | `shesanislandukdri/neuromics_explorer_backend` | 7000 |

Compose files layer: `docker-compose.yml` (base) + `docker-compose.dev.yml` (dev override, host port `1122`, `data_dev` volume, `latest-dev` tag) **or** `docker-compose.prod.yml` (production override, host port `3838`, `data` volume, `latest` tag) + `/srv/neuromics/abebe.{dev,prod}.yml` for team/organisational specific overrides.

## CRITICAL rules

1. **Always pass `-p <project>`** to every `docker compose` command. Without it, `docker compose down` kills **all** services in the default project — including other apps on the shared server. Use `-p nex-dev` for dev and `-p nex-prod` for prod.
2. **Deploys are outward-facing / hard to reverse.** Confirm the tag and environment (dev vs prod) with the user before building or bringing the stack up, unless they already told you to proceed.
3. Run on the **private server** (after the user has pushed local changes to GitHub), not on the local machine — check with the user which host you are on if unsure.
4. This skill does not push images to Docker Hub or run `git push`. Do those only when the user explicitly asks.

## Standard dev deploy

Ask the user for the version tag first (e.g. `1.2.1-dev`) — never reuse an old one silently. Then:

```bash
cd /mnt/Data/neuromics/neuromics_explorer
git checkout dev    # <-- This should only be used if user is not currently on dev branch or to otherwise make sure it is checked out
git pull

DEV_TAG=1.2.1-dev   # <-- confirm with the user, if incorrect then list what was previously used, ask for correct tag and set here

# Build both targets (no cache so R/renv + pip layers are fresh)
docker build --platform linux/amd64 --no-cache --target shiny-frontend \
  -t shesanislandukdri/neuromics_explorer_shiny:${DEV_TAG} .
docker build --platform linux/amd64 --no-cache --target fastapi-backend \
  -t shesanislandukdri/neuromics_explorer_backend:${DEV_TAG} .

# Tag as latest-dev so the compose files can reference a stable name
docker tag shesanislandukdri/neuromics_explorer_shiny:${DEV_TAG}   shesanislandukdri/neuromics_explorer_shiny:latest-dev
docker tag shesanislandukdri/neuromics_explorer_backend:${DEV_TAG} shesanislandukdri/neuromics_explorer_backend:latest-dev

# Recreate ONLY the nex-dev project (‑p keeps other stacks alive) and deploy images
docker compose -p nex-dev down && \
docker compose -p nex-dev -f docker-compose.yml -f docker-compose.dev.yml -f /srv/neuromics/abebe.dev.yml up -d

```

Dev frontend is then reachable on host port **1122** → `http://<server>:1122`.

## Preconditions for prod deploy

```bash
# The image must actually exist locally, ask if user wants to run the below command to check dev tag, then proceed
docker image inspect shesanislandukdri/neuromics_explorer_shiny:${DEV_TAG}   >/dev/null \
  && docker image inspect shesanislandukdri/neuromics_explorer_backend:${DEV_TAG} >/dev/null \
  || { echo "STOP: ${DEV_TAG} not built — deploy dev first"; exit 1; }
```
Do not run any command in the following section until all three hold:
- `docker compose -p nex-dev ps` shows backend healthy and frontend running
- `curl -fsS http://localhost:1122/` succeeds
- The user has explicitly confirmed they are happy with dev and want prod
If any fails, stop and report. Never infer 3 from 1 and 2.


## Prod deploy

Once the development version is ready and the user is satisfied with it, carry out the following tag steps, but tag `latest` (base compose already points frontend/backend at the un-suffixed images which resolve to `latest`), and use the prod override + `-p nex-prod`:

```bash
git checkout main    # <-- Should switch branch to main, then proceed
git pull
DEV_TAG=1.2.1-dev    # <-- confirm with the user, if incorrect then list what was previously used, ask for correct tag and set here
PROD_TAG=1.2.1       # <-- confirm with the user, if incorrect then list what was previously used, ask for correct tag and set here

docker tag shesanislandukdri/neuromics_explorer_shiny:${DEV_TAG}   shesanislandukdri/neuromics_explorer_shiny:${PROD_TAG}
docker tag shesanislandukdri/neuromics_explorer_backend:${DEV_TAG} shesanislandukdri/neuromics_explorer_backend:${PROD_TAG}

docker tag shesanislandukdri/neuromics_explorer_shiny:${PROD_TAG}   shesanislandukdri/neuromics_explorer_shiny:latest
docker tag shesanislandukdri/neuromics_explorer_backend:${PROD_TAG} shesanislandukdri/neuromics_explorer_backend:latest

docker compose -p nex-prod down && \
docker compose -p nex-prod -f docker-compose.yml -f docker-compose.prod.yml -f /srv/neuromics/abebe.prod.yml up -d
```

Prod frontend is on host port **3838**.

## Verify after `up -d`

The backend has a Compose healthcheck (`curl -f http://localhost:7000/health`) and the frontend `depends_on` it being healthy, so a frontend that never starts usually means the backend is unhealthy. Check, in order:

```bash
docker compose -p nex-dev ps                       # checks stack, both should be "running"/"healthy"
docker compose -p nex-dev logs --tail=80 backend   # registry parse + view build should complete
docker compose -p nex-dev logs --tail=80 frontend  # look for "API: http://backend:7000/api" then Shiny listening on 4848
curl -fsS http://localhost:1122/ >/dev/null && echo "frontend OK"   # 3838 for prod
docker ps --format '{{.Names}}  {{.Status}}'       # verify both neighbouring/ backend and frontend are healthy/running

```

## Reporting: 

Report the actual state back to the user — if a container is unhealthy or a log shows a startup error, surface the log lines, declare successes with emoji tick marks.

If any steps fail, stop, report the issue and actual error output, and instead of silently skipping ahead, ask the user if they want to roll back to a previous tag (see-below).

Collate above checks and results into a report under `/mnt/Data/neuromics/logs/deploy_report_$(date +%Y%m%d_%H%M%S).txt` and show the user the path to it. Tail logs shows last 80 lines of container as `/mnt/Data/neuromics/logs/<container>_YYYYMMDD_HHMMSS.log`.

If needed, make directory for logs:
```bash
mkdir -p /mnt/Data/neuromics/logs
```

## Update the data

```bash
cp -r /mnt/Data/neuromics/neuromics_explorer/data /mnt/Data/neuromics/data-backup-$(date +%F)
```

Copy new or updated files into the relevant  folder under `data/` (or `data_dev/`).
If only the data (or data_dev) has changed, re-run the `docker compose ... up -d` command for that environment to deploy and refresh the mounted volume.

## Clean up

Remove dangling, unused images to free disk space once new images are built and deployed. This is safe so tagged images are kept, and rollback targets survive.
```bash
docker image prune -f
```

## Common gotchas

- **renv.lock**: missing or out-of-date/ mismatching `renv.lock` in repo root will break R dependencies in frontend image. Always run `renv::status()` to check any issues when dependencies have changed or aren't working/failing to build image. Use `renv::snapshot()` to update after adding/removing R packages; where appropriately needed use `renv::install("packageName@version")` to install a specific version of a package.
- **Data volume**: both services mount a host directory at `/app/data` in the container. `docker-compose.dev.yml` overrides only the backend to `./data_dev`; the dev frontend still mounts `./data`. Prod leaves both at `./data`. A missing `dataset_registry.yml` or `.duckdb` under the mounted dir makes the backend fail startup — confirm the dir exists and is populated before deploying. Highlight any file differences between `data/` and `data_dev/` to the user when deploying to prod.
- **`--platform linux/amd64`** is required — the server is amd64; building on an Apple-silicon machine without it produces arm64 images that won't run there, hence why it is done through the server.
- **`--no-cache`** guarantees a clean rebuild but is slow (full renv restore + pip install). Drop it only for quick iterations where dependencies are unchanged.
- **Rollback**: re-tag the previous known-good tag as `latest-dev`/`latest` and re-run the `compose ... up -d` line — no rebuild needed due to error with current new build.

--

## Extra notes
- Log in with the password given separately. To switch to key-based login later, run
  `ssh-copy-id dammy@128.40.163.137` from your own machine.
- Your account runs `docker` commands directly — no `sudo` needed.
- The backend build step installs R packages and can take 30+ minutes. That's normal, not stuck.
- Update development first, check it, then ask if happy to deploy production.
- `abebe.prod.yml` and `abebe.dev.yml` set memory/CPU limits and are read-only for you on purpose.
  If a limit needs changing, contact the team's server administrator.
- If anything looks wrong after an update, contact the team's server administrator rather than troubleshooting live on the server.
