import { defineConfig } from "vite";
import path from "path";

export default defineConfig({
  resolve: {
    alias: {
      '@purs-modules': path.resolve(__dirname, "output")
    }
  }
})

