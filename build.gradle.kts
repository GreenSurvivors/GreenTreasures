//import net.minecrell.pluginyml.bukkit.BukkitPluginDescription

plugins {
    `java-library`
    //java
    id("io.papermc.paperweight.userdev") version "1.3.8"
	id("xyz.jpenilla.run-paper") version "1.0.6" // Adds runServer and runMojangMappedServer tasks for testing
}

group = "de.greensurvivors"
version = "0.0.5-SNAPSHOT"
description = "Like TreasureChest but with less bugs!"
//java.sourceCompatibility = JavaVersion.VERSION_17

java {
  // Configure the java toolchain. This allows gradle to auto-provision JDK 17 on systems that only have JDK 8 installed for example.
  toolchain.languageVersion.set(JavaLanguageVersion.of(17))
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
    paperDevBundle("1.19.2-R0.1-SNAPSHOT")
    //get prebuilt from jitpack.io
    //compileOnly("com.github.HenrikoMagnifico","TreasureChestX","8dbc187c09")
    //local installation in case jitpack doesn't work
    compileOnly("com.mtihc.minecraft.treasurechest.v8.plugin","TreasureChest","8.5")
}

tasks {
  // Configure reobfJar to run when invoking the build task
  assemble {
    dependsOn(reobfJar)
  }

  compileJava {
    options.encoding = Charsets.UTF_8.name() // We want UTF-8 for everything

    // Set the release flag. This configures what version bytecode the compiler will emit, as well as what JDK APIs are usable.
    // See https://openjdk.java.net/jeps/247 for more information.
    options.release.set(17)
  }
  
  processResources {
    filteringCharset = Charsets.UTF_8.name() // We want UTF-8 for everything
  }

  /*
  reobfJar {
    // This is an example of how you might change the output location for reobfJar. It's recommended not to do this
    // for a variety of reasons, however it's asked frequently enough that an example of how to do it is included here.
    outputJar.set(layout.buildDirectory.file("libs/PaperweightTestPlugin-${project.version}.jar"))
  }
   */
 
}