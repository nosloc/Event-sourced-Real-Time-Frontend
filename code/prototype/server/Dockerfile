# Build context: the prototype root (see docker-compose.yml)
FROM sbtscala/scala-sbt:eclipse-temurin-21.0.12_8_1.13.0_3.8.4 AS build
WORKDIR /app
COPY . .
RUN --mount=type=cache,target=/root/.cache/coursier \
    --mount=type=cache,target=/root/.ivy2 \
    --mount=type=cache,target=/root/.sbt \
    sbt server/stage

FROM eclipse-temurin:21-jre
WORKDIR /app
COPY --from=build /app/server/target/universal/stage ./
ENV SERVER_HOST=0.0.0.0
EXPOSE 9000
CMD ["bin/server"]
