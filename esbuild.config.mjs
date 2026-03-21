import { cpSync, readdirSync, existsSync, mkdirSync } from 'fs'
import { build } from 'esbuild'

if (!existsSync('./wwwroot/Scripts/')) {
    mkdirSync('./wwwroot/Scripts/', { recursive: true });
}

if (existsSync('./build/')) {
    cpSync('./build/', './wwwroot/Scripts/', { recursive: true });

    const prebundles = readdirSync('./build/');

    prebundles.forEach(file => {
        if (file.endsWith('.js')) {
            const options = {
                entryPoints: ['./build/' + file],
                bundle: true,
                minify: true,
                format: 'iife',
                outfile: 'wwwroot/Scripts/' + file,
                globalName: 'wsbundle'
            };

            console.log('Bundling:', file);
            build(options);
        }
    });

    if (existsSync('./build/workers/')) {
        if (!existsSync('./wwwroot/Scripts/workers/')) {
            mkdirSync('./wwwroot/Scripts/workers/', { recursive: true });
        }

        const workers = readdirSync('./build/workers/');

        workers.forEach(file => {
            if (file.endsWith('.js')) {
                const options = {
                    entryPoints: ['./build/workers/' + file],
                    bundle: true,
                    minify: true,
                    format: 'iife',
                    outfile: 'wwwroot/Scripts/workers/' + file,
                };

                console.log('Bundling worker:', file);
                build(options);
            }
        });
    }
} else {
    console.log('Skipping esbuild step: ./build/ does not exist');
}