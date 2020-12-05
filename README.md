# saufautomat

A website which supports your and your friends alcoholism.

If you want this on an android smartphone, take a look at [OpenAlcoholics/saufautomobil](https://github.com/OpenAlcoholics/saufautomobil).

## Tasks

Tasks are currently only curated in german, if you want to contribute some english tasks/fix them up you can do this in [OpenAlcoholics/drinking-game-cards](https://github.com/OpenAlcoholics/drinking-game-cards).

## Building

Run `dotnet tool restore`.

Use `dotnet fable src --run webpack-dev-server` for a development build and `dotnet fable src --run webpack` for a production build (output in `src/bundle.js`).

The development server is available at 'http://0.0.0.0:8080'.

### Dependencies

- node + npm (tested with node 14.2.0)
  - npm install
- dotnet sdk (tested with 3.1.103)
