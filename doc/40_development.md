# Development

[![CircleCI](https://circleci.com/gh/randomseed-io/bankster.svg?style=svg)](https://circleci.com/gh/randomseed-io/bankster)

## Source code

* https://github.com/randomseed-io/bankster

## Common tasks

### Building docs

```bash
make docs
```

### Generating reader handlers for tagged literals

```bash
make readers
```

### Building JAR

```bash
make jar
```

### Rebuilding POM

```bash
make pom
```

### Signing POM

```bash
make sig
```

### Deploying to Clojars

```bash
make deploy
```

### Interactive development

```bash
bin/repl
```

Starts REPL and nREPL server (port number is stored in `.nrepl-port`).

### Testing

```bash
bin/test
```
