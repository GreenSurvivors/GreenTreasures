plugins {
    `java-library`
    id("io.papermc.paperweight.userdev") version "2.0.0-beta.18"
    id("xyz.jpenilla.run-paper") version "3.0.2"
}

group = "de.greensurvivors"
description = "Like TreasureChest but with less bugs!"
version = buildString {
    append(project.properties["plugin_version"])

    if ((project.properties["is_release"] as String).toBoolean().not()) {
        append("-Snapshot")
    }

    append("+${project.properties["minecraft_version"]}")
}

// todo remove with 26.1
// we only work with paper and downstream!
paperweight.reobfArtifactConfiguration = io.papermc.paperweight.userdev.ReobfArtifactConfiguration.MOJANG_PRODUCTION

java {
    // Configure the java toolchain. This allows gradle to auto-provision JDK 21 on systems that only have JDK 8 installed for example.
    toolchain.languageVersion = JavaLanguageVersion.of("${rootProject.properties["java_version"]}")
    sourceCompatibility = JavaVersion.toVersion(rootProject.properties["java_version"]!!)
}

repositories {
    mavenLocal()
    mavenCentral()

    //paper
    maven {
        url = uri("https://repo.papermc.io/repository/maven-public/")
    }
    maven {
        url = uri("https://maven.enginehub.org/repo/")
    }
}

dependencies {
    paperweight.paperDevBundle("${project.properties["minecraft_version"]}-R0.1-SNAPSHOT")
    compileOnly("com.sk89q.worldguard:worldguard-bukkit:${project.properties["worldGuard_version"]}")

    compileOnly("com.zaxxer:HikariCP:${project.properties["hikariCP_version"]}")
    compileOnly("com.github.ben-manes.caffeine:caffeine:${project.properties["caffeine_version"]}") // caches
    compileOnly("org.apache.commons:commons-collections4:${project.properties["commonsCollections_version"]}")
    compileOnly("com.github.f4b6a3:ulid-creator:${project.properties["ulidCreator_version"]}")
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
        options.release = project.properties["java_version"].toString().toInt()
    }

    javadoc {
        options.encoding = Charsets.UTF_8.name()
    }

    runServer {
        downloadPlugins {
            // make sure to double-check the version id on the Modrinth version page
            modrinth("worldedit", project.properties["worldEdit_runVersion"].toString())
            modrinth("worldguard", project.properties["worldGuard_runVersion"].toString())
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