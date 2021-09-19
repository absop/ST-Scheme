# Scheme

Scheme syntax highlighting and code formatter for Sublime Text 4.


## Screenshots
![1](images/expression-comment.png)

![2](images/2.png)

![3](images/quasiquote.png)


## Usage
### Syntax Highlighting

To distinguish expression commented regions from normal regions, exec the command `UI: Customize Color Scheme`, and merge the following code into your customized color scheme.

```json
{
    "rules":
    [
        {
          "scope": "meta.comment",
          "foreground_adjust": "l(- 15%) s(- 30%)",
          "background": "var(--background)"
        }
    ]
}
```

### Code Formatting

|           key           | Command            | Context       |
| :---------------------: | ------------------ | ------------- |
| <kbd> ctrl+alt+f </kbd> | Format Scheme Code | source.scheme |
