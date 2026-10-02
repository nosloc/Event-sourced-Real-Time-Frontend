import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";

export default defineConfig({
    plugins: [
        scalaJSPlugin({
            cwd: "..",           // where build.sbt is
            projectID: "client"  // the Scala.js project's name in build.sbt
        })
    ],
    server: {
        port: 3000,
        proxy: {
            "/api": { target: "http://127.0.0.1:9000" }   // forwards /api to your server
        }
    }
});