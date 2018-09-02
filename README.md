# Map-reduce language interpreter

## How to build and run

You'll need a [mill](http://www.lihaoyi.com/mill/) to build a project

Build current version and run example program:

```
mill root.run example/program.mr

```

Build stand-alone assembly and run example program:

```
mill root.assembly
out/root/assembly/dest/out.jar example/program.mr
```
