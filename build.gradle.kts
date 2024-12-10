//import net.minecrell.pluginyml.bukkit.BukkitPluginDescription

plugins {
    `java-library`
    //java
    id("io.papermc.paperweight.userdev") version "1.7.3"
    id("xyz.jpenilla.run-paper") version "2.3.0"
}

group = "de.greensurvivors"
version = "0.1.2-SNAPSHOT"
description = "Like TreasureChest but with less bugs!"
val mcVersion by extra("1.21.4")

// we only work with paper and downstream!
paperweight.reobfArtifactConfiguration = io.papermc.paperweight.userdev.ReobfArtifactConfiguration.MOJANG_PRODUCTION

java {
  // Configure the java toolchain. This allows gradle to auto-provision JDK 17 on systems that only have JDK 8 installed for example.
    toolchain.languageVersion.set(JavaLanguageVersion.of(21))
}

repositories {
    mavenLocal()

    //paper
    maven {
        url = uri("https://repo.papermc.io/repository/maven-public/")
    }

    //prebuilt jars from public gits
    maven {
        url = uri("https://jitpack.io/")
    }
}

dependencies {
    paperweight.paperDevBundle("$mcVersion-R0.1-SNAPSHOT")
    compileOnly("com.zaxxer:HikariCP:6.0.0")
    compileOnly("com.github.ben-manes.caffeine:caffeine:3.1.8") // caches
    compileOnly("org.yaml:snakeyaml:2.3")
}

tasks {
  compileJava {
    options.encoding = Charsets.UTF_8.name() // We want UTF-8 for everything

    // Set the release flag. This configures what version bytecode the compiler will emit, as well as what JDK APIs are usable.
    // See https://openjdk.java.net/jeps/247 for more information.
      options.release.set(21)
  }
  
  processResources {
    filteringCharset = Charsets.UTF_8.name() // We want UTF-8 for everything

      expand(project.properties)
  }
}