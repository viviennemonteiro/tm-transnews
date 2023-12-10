## rolling up remaining
```{r}
# roll up differing source type
metadata_rolled <- deduped_df |>
  group_by(title, source_type) |>
  summarise(
    across(!full_text,                                  
           ~paste0(unique(na.omit(.x)), collapse = "; "))
  )

```

```{r}
metadata_rolled |>
  filter(
    grepl("; ", document_type)
  )

metadata_rolled |>
  get_dupes()
```

```{r}
metadata_rolled |>
  summary()
```