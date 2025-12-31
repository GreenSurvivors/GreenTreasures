package de.greensurvivors.greentreasure.legacy;

import org.jetbrains.annotations.NotNull;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

public class ForkingSuccessCollector {
    protected final @NotNull AtomicInteger processesRunning = new AtomicInteger(0);
    protected volatile boolean allProcessesStarted = false;
    protected volatile boolean allSuccess = true;

    protected final @NotNull Consumer<@NotNull Boolean> successConsumer;

    protected ForkingSuccessCollector(@NotNull Consumer<@NotNull Boolean> successConsumer) {
        this.successConsumer = successConsumer;
    }

    public void fork() {
        processesRunning.incrementAndGet();
    }

    public void finish(final boolean success) {
        if (!success) {
            allSuccess = false;
        }

        if (processesRunning.decrementAndGet() <= 0 && allProcessesStarted) {
            successConsumer.accept(allSuccess);
        }
    }

    public void forkFinish(final boolean success) {
        if (!success) {
            allSuccess = false;
        }

        if (processesRunning.get() <= 0 && allProcessesStarted) {
            successConsumer.accept(allSuccess);
        }
    }

    public void markAllStarted() {
        allProcessesStarted = true;
    }
}
