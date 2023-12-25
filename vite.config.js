import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  plugins: [elmPlugin()],
  server: {
    proxy: {
      "/api": {
        target: "http://localhost:8000", // python http server
        rewrite: (path) => `${path.replace(/\/+$/, "")}.json`,
      },
      // "/api": {
      //   target: "http://localhost:8080", // real yorksap server
      // },
      "/ws": {
        target: "ws://localhost:8001",
        ws: true,
      },
    },
  },
});
