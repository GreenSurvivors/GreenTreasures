package de.greensurvivors.greentreasure.legacy;

import de.greensurvivors.greentreasure.GreenTreasure;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.util.concurrent.atomic.AtomicInteger;

abstract class ALegacyFileVisitor extends SimpleFileVisitor<@NotNull Path> {
    protected final @NotNull ForkingSuccessCollector successCollector;
    protected final @NotNull AtomicInteger importProcessId;
    protected final @NotNull GreenTreasure plugin;

    protected ALegacyFileVisitor(final @NotNull GreenTreasure plugin, final @NotNull AtomicInteger importProcessId, final @NotNull ForkingSuccessCollector successCollector) {
        this.successCollector = successCollector;
        this.importProcessId = importProcessId;
        this.plugin = plugin;
    }

    public @NotNull FileVisitResult visitFileFailed(final @NotNull Path filePath, final @NotNull IOException exc) {
        successCollector.forkFinish(false);
        plugin.getComponentLogger().warn("Could not read file at {}", filePath, exc);

        return FileVisitResult.CONTINUE;
    }
}
