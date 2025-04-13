import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [elmPlugin({ debug: false, optimize: true })],
    server: {
        hmr: {
            clientPort: 443,
        },
        proxy: {
            '/ws': {
                target: 'http://127.0.0.1:3000/ws',
                ws: true,
                changeOrigin: true,
            },
        },
    },
});
