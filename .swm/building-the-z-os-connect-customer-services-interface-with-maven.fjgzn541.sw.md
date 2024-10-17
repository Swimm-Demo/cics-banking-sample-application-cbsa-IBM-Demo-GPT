---
title: Building the Z-OS Connect Customer Services Interface with Maven
---
# Intro

This document explains how Maven is used in the <SwmPath>[src/Z-OS-Connect-Customer-Services-Interface/](src/Z-OS-Connect-Customer-Services-Interface/)</SwmPath> directory. It will cover the Maven configuration and its usage in building the project.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="1">

---

# Project Metadata

The <SwmPath>[src/Z-OS-Connect-Customer-Services-Interface/pom.xml](src/Z-OS-Connect-Customer-Services-Interface/pom.xml)</SwmPath> file begins with the XML declaration and project metadata, including the XML namespace and schema location for Maven POM.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright IBM Corp. 2023 -->
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 https://maven.apache.org/xsd/maven-4.0.0.xsd">
	<modelVersion>4.0.0</modelVersion>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="6">

---

# Parent Project

The project inherits from the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="8:4:10" line-data="		&lt;artifactId&gt;spring-boot-starter-parent&lt;/artifactId&gt;">`spring-boot-starter-parent`</SwmToken> POM, which provides default configurations for Spring Boot applications.

```xml
	<parent>
		<groupId>org.springframework.boot</groupId>
		<artifactId>spring-boot-starter-parent</artifactId>
		<version>3.2.5</version>
		<relativePath /> <!-- lookup parent from repository -->
	</parent>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="14">

---

# Project Information

Basic project information such as <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="14:2:2" line-data="	&lt;groupId&gt;com.ibm.cics.cip.bank.springboot&lt;/groupId&gt;">`groupId`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="15:2:2" line-data="	&lt;artifactId&gt;customerservices&lt;/artifactId&gt;">`artifactId`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="16:2:2" line-data="	&lt;version&gt;1.0&lt;/version&gt;">`version`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="17:2:2" line-data="	&lt;packaging&gt;war&lt;/packaging&gt;">`packaging`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="18:2:2" line-data="	&lt;name&gt;customerservices&lt;/name&gt;">`name`</SwmToken>, and <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="19:2:2" line-data="	&lt;description&gt;Springboot project utilising Z/OS Connect&lt;/description&gt;">`description`</SwmToken> are defined.

```xml
	<groupId>com.ibm.cics.cip.bank.springboot</groupId>
	<artifactId>customerservices</artifactId>
	<version>1.0</version>
	<packaging>war</packaging>
	<name>customerservices</name>
	<description>Springboot project utilising Z/OS Connect</description>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="20">

---

# Properties

The Java version is specified in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="20:2:2" line-data="	&lt;properties&gt;">`properties`</SwmToken> section to ensure compatibility across different environments.

```xml
	<properties>
		<java.version>17</java.version>
	</properties>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="23">

---

# Dependencies

Various dependencies required for the project are listed, including libraries for JSON processing, Netty for networking, Spring Boot starters, and validation APIs.

```xml
	<dependencies>

		<dependency>
			<groupId>com.beust</groupId>
			<artifactId>jcommander</artifactId>
			<version>1.82</version>
			<scope>compile</scope>
		</dependency>

		<dependency>
			<groupId>com.fasterxml.jackson.core</groupId>
			<artifactId>jackson-core</artifactId>
			<scope>compile</scope>
		</dependency>

		<dependency>
			<groupId>com.fasterxml.jackson.core</groupId>
			<artifactId>jackson-databind</artifactId>
		</dependency>

		<dependency>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="150">

---

# Build Plugins

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="150:2:2" line-data="	&lt;build&gt;">`build`</SwmToken> section includes plugins for building the project. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="154:4:10" line-data="				&lt;artifactId&gt;spring-boot-maven-plugin&lt;/artifactId&gt;">`spring-boot-maven-plugin`</SwmToken> is used to package the Spring Boot application, and the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="158:4:10" line-data="				&lt;artifactId&gt;cics-bundle-maven-plugin&lt;/artifactId&gt;">`cics-bundle-maven-plugin`</SwmToken> is used to bundle the WAR file for deployment on z/OS.

```xml
	<build>
		<plugins>
			<plugin>
				<groupId>org.springframework.boot</groupId>
				<artifactId>spring-boot-maven-plugin</artifactId>
			</plugin>
			<plugin>
				<groupId>com.ibm.cics</groupId>
				<artifactId>cics-bundle-maven-plugin</artifactId>
				<version>1.0.2</version>
				<executions>
					<execution>
						<goals>
							<goal>bundle-war</goal>
						</goals>
						<configuration>
							<jvmserver>CBSAWLP</jvmserver>
						</configuration>
					</execution>
				</executions>
			</plugin>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/mvnw" line="1">

---

# Maven Wrapper Script

The `mvnw` script is a shell script that ensures the correct version of Maven is used to build the project. It checks for required environment variables and sets up the Maven environment.

```
#!/bin/sh
#
# ----------------------------------------------------------------------------
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
# ----------------------------------------------------------------------------

```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/mvnw.cmd" line="1">

---

# Maven Wrapper Batch Script

The <SwmPath>[src/Z-OS-Connect-Customer-Services-Interface/mvnw.cmd](src/Z-OS-Connect-Customer-Services-Interface/mvnw.cmd)</SwmPath> script is a batch script for Windows that performs similar functions to the `mvnw` shell script, ensuring the correct version of Maven is used and setting up the environment.

```batchfile
@REM ----------------------------------------------------------------------------
@REM Licensed to the Apache Software Foundation (ASF) under one
@REM or more contributor license agreements.  See the NOTICE file
@REM distributed with this work for additional information
@REM regarding copyright ownership.  The ASF licenses this file
@REM to you under the Apache License, Version 2.0 (the
@REM "License"); you may not use this file except in compliance
@REM with the License.  You may obtain a copy of the License at
@REM
@REM    https://www.apache.org/licenses/LICENSE-2.0
@REM
@REM Unless required by applicable law or agreed to in writing,
@REM software distributed under the License is distributed on an
@REM "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
@REM KIND, either express or implied.  See the License for the
@REM specific language governing permissions and limitations
@REM under the License.
@REM ----------------------------------------------------------------------------

@REM ----------------------------------------------------------------------------
@REM Maven Start Up Batch script
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/mvnw.cmd" line="121">

---

# Download URL for Maven Wrapper

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/mvnw.cmd" pos="123:2:2" line-data="set DOWNLOAD_URL=&quot;https://repo.maven.apache.org/maven2/io/takari/maven-wrapper/0.5.6/maven-wrapper-0.5.6.jar&quot;">`DOWNLOAD_URL`</SwmToken> variable specifies the URL from which the Maven Wrapper JAR can be downloaded if it is not already present.

```batchfile
set WRAPPER_LAUNCHER=org.apache.maven.wrapper.MavenWrapperMain

set DOWNLOAD_URL="https://repo.maven.apache.org/maven2/io/takari/maven-wrapper/0.5.6/maven-wrapper-0.5.6.jar"

FOR /F "tokens=1,2 delims==" %%A IN ("%MAVEN_PROJECTBASEDIR%\.mvn\wrapper\maven-wrapper.properties") DO (
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/mvnw.cmd" line="113">

---

# JVM Configuration

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/mvnw.cmd" pos="114:34:34" line-data="for /F &quot;usebackq delims=&quot; %%a in (&quot;%MAVEN_PROJECTBASEDIR%\.mvn\jvm.config&quot;) do set JVM_CONFIG_MAVEN_PROPS=!JVM_CONFIG_MAVEN_PROPS! %%a">`JVM_CONFIG_MAVEN_PROPS`</SwmToken> variable concatenates all lines from the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/mvnw.cmd" pos="114:24:26" line-data="for /F &quot;usebackq delims=&quot; %%a in (&quot;%MAVEN_PROJECTBASEDIR%\.mvn\jvm.config&quot;) do set JVM_CONFIG_MAVEN_PROPS=!JVM_CONFIG_MAVEN_PROPS! %%a">`jvm.config`</SwmToken> file, which contains JVM options for Maven.

```batchfile
@setlocal EnableExtensions EnableDelayedExpansion
for /F "usebackq delims=" %%a in ("%MAVEN_PROJECTBASEDIR%\.mvn\jvm.config") do set JVM_CONFIG_MAVEN_PROPS=!JVM_CONFIG_MAVEN_PROPS! %%a
@endlocal & set JVM_CONFIG_MAVEN_PROPS=%JVM_CONFIG_MAVEN_PROPS%
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
