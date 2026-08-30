package de.greensurvivors.greentreasure;

import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;

public class UncaughtExceptionHandler implements Thread.UncaughtExceptionHandler {
    private final @NotNull Logger logger;

    public UncaughtExceptionHandler(final @NotNull Logger logger) {
        this.logger = logger;
    }

    @Override
    public void uncaughtException(final @NotNull Thread thread, final @NotNull Throwable throwable) {
        this.logger.error("Caught previously unhandled exception :");
        this.logger.error(thread.getName(), throwable);
    }
}
