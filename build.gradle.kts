plugins {
    `java-library`
    id("io.papermc.paperweight.userdev") version "2.0.0-beta.14"
    id("xyz.jpenilla.run-paper") version "2.3.1"
}

group = "de.greensurvivors"
version = "0.2.0-SNAPSHOT"
description = "Like TreasureChest but with less bugs!"
val mcVersion by extra("1.21.4")
val javaVersion = 21

// we only work with paper and downstream!
paperweight.reobfArtifactConfiguration = io.papermc.paperweight.userdev.ReobfArtifactConfiguration.MOJANG_PRODUCTION

// dependencies
val worldGuardDependencyVersion by extra("7.0.13-SNAPSHOT") // this is the version used to compile against. It is potentially not a runnable plugin, therefor we specify the modrinth version below
val worldGuardRunVersion by extra("7.0.13-beta-2") // note this is the modrinth version, used in the run task
val worldEditVersion by extra("HIoAq6RI") // wierd modrinth version id, used to fetch worldEdit for the run task.
val hikariCPVersion by extra("6.2.1")
val caffeineVersion by extra("3.1.8")
val snakeyamlVersion by extra("2.3")
val commonsCollectionVersion by extra("4.5.0-M3")
val ulidCreatorVersion by extra("5.2.3")

java {
  // Configure the java toolchain. This allows gradle to auto-provision JDK 21 on systems that only have JDK 8 installed for example.
    toolchain.languageVersion.set(JavaLanguageVersion.of(javaVersion))
}

repositories {
    mavenLocal()

    //paper
    maven {
        url = uri("https://repo.papermc.io/repository/maven-public/")
    }
    maven {
        url = uri("https://maven.enginehub.org/repo/")
    }
}

dependencies {
    paperweight.paperDevBundle("$mcVersion-R0.1-SNAPSHOT")
    compileOnly("com.sk89q.worldguard:worldguard-bukkit:$worldGuardDependencyVersion")

    compileOnly("com.zaxxer:HikariCP:$hikariCPVersion")
    compileOnly("com.github.ben-manes.caffeine:caffeine:$caffeineVersion") // caches
    compileOnly("org.yaml:snakeyaml:$snakeyamlVersion")
    compileOnly("org.apache.commons:commons-collections4:$commonsCollectionVersion")
    compileOnly("com.github.f4b6a3:ulid-creator:$ulidCreatorVersion")
}

tasks {

    processResources {
        filteringCharset = Charsets.UTF_8.name() // We want UTF-8 for everything

        expand(project.properties)
    }
  compileJava {
    options.encoding = Charsets.UTF_8.name() // We want UTF-8 for everything

    // Set the release flag. This configures what version bytecode the compiler will emit, as well as what JDK APIs are usable.
    // See https://openjdk.java.net/jeps/247 for more information.
      options.release.set(javaVersion)
  }

    runServer {
        downloadPlugins {
            // make sure to double-check the version id on the Modrinth version page
            modrinth("worldedit", worldEditVersion)
            modrinth("worldguard", worldGuardRunVersion)
        }

        // disable bstats, as it isn't needed for dev environment
        doFirst { // this happens after downloading the plugins above, but before the server starts
            val cfg = runDirectory.get().asFile.resolve("plugins/bStats/config.yml")
            if (!cfg.exists()) {
                cfg.parentFile.mkdirs()
                cfg.createNewFile()
            }
            cfg.writeText("enabled: false\n")
        }
        // automatically agree to eula
        jvmArgs("-Dcom.mojang.eula.agree=true")
    }
}