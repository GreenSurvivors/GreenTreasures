plugins {
    `java-library`
    id("io.papermc.paperweight.userdev") version "2.0.0-beta.21"
    id("xyz.jpenilla.run-paper") version "3.0.2"
}

group = "de.greensurvivors"
description = "Like TreasureChest but with less bugs!"
version = buildString {
    append(getProperty("plugin_version"))

    if (getProperty("is_release").toBoolean().not()) {
        append("-Snapshot")
    }

    append("+${getProperty("minecraft_version")}")
}

java {
    // Configure the java toolchain. This allows gradle to auto-provision JDK 21 on systems that only have JDK 8 installed for example.
    toolchain.languageVersion = JavaLanguageVersion.of(getProperty("java_version"))
    sourceCompatibility = JavaVersion.toVersion(getProperty("java_version"))
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
    paperweight.paperDevBundle("${getProperty("minecraft_version")}.build.+")
    compileOnly("com.sk89q.worldguard:worldguard-bukkit:${getProperty("worldGuard_version")}") {
        exclude("com.google.guava", "guava")
        exclude("com.google.code.gson", "gson")
    }

    compileOnly("com.zaxxer:HikariCP:${getProperty("hikariCP_version")}")
    compileOnly("com.github.ben-manes.caffeine:caffeine:${getProperty("caffeine_version")}") // caches
    compileOnly("com.github.f4b6a3:ulid-creator:${getProperty("ulidCreator_version")}")
}

tasks {
    processResources {
        filteringCharset = Charsets.UTF_8.name() // We want UTF-8 for everything

        expand(providers.gradlePropertiesPrefixedBy("")
            .get()
            .toMutableMap() // f you gradle for being inconvenient in newer versions
            .plus("version" to project.version)
            .plus("description" to project.description)
            .plus("group" to project.group))
    }

    compileJava {
        options.encoding = Charsets.UTF_8.name() // We want UTF-8 for everything

        // Set the release flag. This configures what version bytecode the compiler will emit, as well as what JDK APIs are usable.
        // See https://openjdk.java.net/jeps/247 for more information.
        options.release = getProperty("java_version").toInt()
    }

    javadoc {
        options.encoding = Charsets.UTF_8.name()
    }

    runServer {
        downloadPlugins {
            // make sure to double-check the version id on the Modrinth version page
            modrinth("worldedit", getProperty("worldEdit_runVersion"))
            modrinth("worldguard", getProperty("worldGuard_runVersion"))
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

private fun getProperty(value: String): String = providers.gradleProperty(value).get()