import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import tailwindcss from "@tailwindcss/vite";
import wasm from "vite-plugin-wasm";
import { viteStaticCopy } from "vite-plugin-static-copy";

// https://vite.dev/config/
export default defineConfig({
	plugins: [
		react(),
		tailwindcss(),
		wasm(),
		viteStaticCopy({
			targets: [
				{
					src: "./pkg/*.wasm",
					dest: ".",
				},
			],
		}),
	],
	server: {
		fs: {
			allow: [".."],
		},
	},
	build: {
		target: "esnext",
	},
	optimizeDeps: {
		exclude: ["../../pkg"],
		esbuildOptions: {
			target: "esnext",
		},
	},
});
