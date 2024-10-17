---
title: Setting Company Name (GETCOMPY program)
---
This document will cover the GETCOMPY program. We'll cover:

1. What the Program Does
2. Program Flow
3. Program Sections

## What the Program Does

The GETCOMPY program is designed to set the company name for the CICS Bank Sample Application. It achieves this by moving a predefined string 'CICS Bank Sample Application' into the <SwmToken path="src/base/cobol_src/GETCOMPY.cbl" pos="38:15:17" line-data="           move &#39;CICS Bank Sample Application&#39; to COMPANY-NAME.">`COMPANY-NAME`</SwmToken> variable and then returning control to CICS.

## Program Flow

This is a visualization of the flow:

<SwmSnippet path="/src/base/cobol_src/GETCOMPY.cbl" line="36">

---

### PREMIERE SECTION

First, the program sets the company name by moving the string 'CICS Bank Sample Application' into the <SwmToken path="src/base/cobol_src/GETCOMPY.cbl" pos="38:15:17" line-data="           move &#39;CICS Bank Sample Application&#39; to COMPANY-NAME.">`COMPANY-NAME`</SwmToken> variable. Then, it returns control to CICS using the <SwmToken path="src/base/cobol_src/GETCOMPY.cbl" pos="40:1:5" line-data="           EXEC CICS RETURN">`EXEC CICS RETURN`</SwmToken> statement.

```cobol
       PREMIERE SECTION.
       A010.
           move 'CICS Bank Sample Application' to COMPANY-NAME.

           EXEC CICS RETURN
           END-EXEC.
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
