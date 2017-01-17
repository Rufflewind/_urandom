

iso_date() {
    date -u +"%Y-%m-%dT%H:%M:%SZ"
}

# ideas:
# some sort of preprocessor to deal with convert code that uses local
# variables into code using a stack; maybe use your [ ] lang ?
# var[x, y] # means everything is scoped within the { } or ( )
