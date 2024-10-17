---
title: Building the WebUI with Maven
---
# Intro

This document explains how Maven is used in the <SwmPath>[src/webui/](src/webui/)</SwmPath> directory to build the WebUI component of the project.

<SwmSnippet path="/src/webui/pom.xml" line="1">

---

# Project Metadata

The <SwmPath>[src/webui/pom.xml](src/webui/pom.xml)</SwmPath> file begins with the project metadata, including the group ID, artifact ID, version, and packaging type. This information uniquely identifies the project and specifies that the output will be a WAR file.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright IBM Corp. 2023 -->
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
	<modelVersion>4.0.0</modelVersion>

	<groupId>com.ibm.cics.cip.bank.libertyapi.webui</groupId>
	<artifactId>webui</artifactId>
	<version>1.0</version>

	<name>webui</name>
	<url>https://github.com/cicsdev/cics-banking-sample-application-cbsa</url>
	<packaging>war</packaging>
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/pom.xml" line="15">

---

# Project Properties

The properties section defines the source encoding and the Java compiler version to be used. This ensures consistency in the build process.

```xml
	<properties>
		<project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
		<maven.compiler.source>8</maven.compiler.source>
		<maven.compiler.target>8</maven.compiler.target>
	</properties>
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/pom.xml" line="22">

---

# Dependency Management

The dependency management section imports a Bill of Materials (BOM) for IBM CICS dependencies. This centralizes the version management of these dependencies.

```xml
	<dependencyManagement>
		<dependencies>
			<dependency>
				<groupId>com.ibm.cics</groupId>
				<artifactId>com.ibm.cics.ts.bom</artifactId>
				<version>5.6-20200609123739</version>
				<type>pom</type>
				<scope>import</scope>
			</dependency>
		</dependencies>
	</dependencyManagement>
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/pom.xml" line="34">

---

# Dependencies

The dependencies section lists all the libraries required by the project. These include Jakarta EE APIs, validation APIs, servlet APIs, and various IBM-specific libraries. Some dependencies are marked as 'provided' because they are expected to be available in the runtime environment.

```xml
	<dependencies>
        <dependency>
            <groupId>jakarta.platform</groupId>
            <artifactId>jakarta.jakartaee-api</artifactId>
            <version>10.0.0</version>
            <scope>provided</scope>
        </dependency>
		<dependency>
			<groupId>javax.ws.rs</groupId>
			<artifactId>javax.ws.rs-api</artifactId>
			<version>2.1.1</version>
			<scope>provided</scope>
		</dependency>
		<dependency>
			<groupId>javax.validation</groupId>
			<artifactId>validation-api</artifactId>
			<version>2.0.1.Final</version>
			<scope>provided</scope>
		</dependency>

		<dependency>
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/pom.xml" line="99">

---

# Build Configuration

The build section specifies the source and output directories for the compiled classes. This is where Maven will place the compiled bytecode.

```xml
	<build>
		<sourceDirectory>${basedir}/src/main/java</sourceDirectory>
		<outputDirectory>${basedir}/target/classes</outputDirectory>
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/pom.xml" line="102">

---

# Plugin Management

The plugin management section locks down the versions of various Maven plugins to ensure consistent builds. This includes plugins for cleaning, compiling, packaging, and deploying the project.

```xml
		<pluginManagement>
			<!-- lock down plugins versions to avoid using Maven defaults (may be 
				moved to parent pom) -->
			<plugins>
				<!-- clean lifecycle, see https://maven.apache.org/ref/current/maven-core/lifecycles.html#clean_Lifecycle -->
				<plugin>
					<artifactId>maven-clean-plugin</artifactId>
					<version>3.1.0</version>
				</plugin>
				<!-- default lifecycle, jar packaging: see https://maven.apache.org/ref/current/maven-core/default-bindings.html#Plugin_bindings_for_jar_packaging -->
				<plugin>
					<artifactId>maven-resources-plugin</artifactId>
					<version>3.0.2</version>
				</plugin>
				<plugin>
					<artifactId>maven-compiler-plugin</artifactId>
					<version>3.8.0</version>
					<configuration>
						<compilerArgs>
							<arg>-Xlint:deprecation</arg>
							<arg>-Xlint:unchecked</arg>
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/pom.xml" line="151">

---

# WAR Plugin Configuration

The Maven WAR plugin is configured to include the <SwmPath>[src/webui/WebContent/WEB-INF/web.xml](src/webui/WebContent/WEB-INF/web.xml)</SwmPath> file and other web resources from the <SwmToken path="src/webui/pom.xml" pos="156:4:4" line-data="						&lt;webXml&gt;WebContent\WEB-INF\web.xml&lt;/webXml&gt;">`WebContent`</SwmToken> directory. This ensures that the WAR file is correctly structured for deployment.

```xml
				<plugin>
					<groupId>org.apache.maven.plugins</groupId>
					<artifactId>maven-war-plugin</artifactId>
					<version>3.4.0</version>
					<configuration>
						<webXml>WebContent\WEB-INF\web.xml</webXml>
						<webResources>
							<resource>
								<directory>WebContent</directory>
							</resource>
						</webResources>
					</configuration>
				</plugin>
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
