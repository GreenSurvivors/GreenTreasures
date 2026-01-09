package de.greensurvivors.greentreasure.config;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ConfigOption<T> {
    private final @NotNull String path;
    protected final T fallback;
    protected volatile @Nullable T value = null;

    public ConfigOption(final @NotNull String path, final T fallback) {
        this.path = path;
        this.fallback = fallback;
    }

    public T getValueOrFallback() {
        return value == null ? fallback : value;
    }

    public @NotNull String getPath() {
        return path;
    }

    public void setValue(final @Nullable T newValue) {
        value = newValue;
    }
}
