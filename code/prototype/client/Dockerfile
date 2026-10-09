# Build context: the prototype root (see docker-compose.yml)
FROM sbtscala/scala-sbt:eclipse-temurin-21.0.12_8_1.13.0_3.8.4 AS build
RUN apt-get update && apt-get install -y --no-install-recommends nodejs npm \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY . .
WORKDIR /app/client
# vite build makes the Scala.js plugin run sbt fullLinkJS
RUN --mount=type=cache,target=/root/.cache/coursier \
    --mount=type=cache,target=/root/.ivy2 \
    --mount=type=cache,target=/root/.sbt \
    npm ci && npm run build

FROM nginx:alpine
COPY client/nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=build /app/client/dist /usr/share/nginx/html
