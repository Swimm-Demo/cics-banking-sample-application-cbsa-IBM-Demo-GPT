---
title: Setting Sort Code (GETSCODE program)
---
This document will cover the GETSCODE program. We'll cover:

1. What the Program Does
2. Program Flow
3. Program Sections

## What the Program Does

The GETSCODE program is designed to move a literal sort code into the sort code field of the DFHCOMMAREA. This is a simple operation that ensures the sort code is correctly set in the communication area for further processing by other programs or transactions.

## Program Flow

This is a visualization of the flow:

<SwmSnippet path="/src/base/cobol_src/GETSCODE.cbl" line="37">

---

### PREMIERE SECTION

First, the program moves the literal sort code into the sort code field of the DFHCOMMAREA. This ensures that the sort code is correctly set for further processing. Then, the program returns control to CICS using the <SwmToken path="src/base/cobol_src/GETSCODE.cbl" pos="43:1:5" line-data="           EXEC CICS RETURN">`EXEC CICS RETURN`</SwmToken> statement.

```cobol
       PREMIERE SECTION.
       A010.
           MOVE LITERAL-SORTCODE
           TO SORTCODE OF DFHCOMMAREA.


           EXEC CICS RETURN
           END-EXEC.

```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
