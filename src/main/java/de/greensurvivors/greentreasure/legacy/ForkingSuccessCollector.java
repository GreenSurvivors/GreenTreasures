package de.greensurvivors.greentreasure.legacy;

import org.jetbrains.annotations.NotNull;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

// man this description hits to close to home
/// this class keeps track of a thread and the children it may or may not spawn.
/// if at anytime anywhere a child (branch) fails.
/// If so, the whole state fails.
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

    /// finishes a branch with a success value
    public void finish(final boolean success) {
        if (!success) {
            allSuccess = false;
        }

        if (processesRunning.decrementAndGet() <= 0 && allProcessesStarted) {
            successConsumer.accept(allSuccess);
        }
    }

    /// finishes with a success value, without messing with the amount of branches.
    /// This is the same as calling fork() and finish() right after another.
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
