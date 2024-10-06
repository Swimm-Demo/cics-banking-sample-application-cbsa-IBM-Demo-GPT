---
title: Program ACCTCTRL
---
The ACCTCTRL program is used to control accounts in a banking application. The document will cover the purpose of the program, its flow, and the main sections.

## What the Program Does

The program is used to control accounts in a banking application. It retrieves the number of accounts for a given sort code from a database and returns the result.

## Program Flow

<SwmSnippet path="src/base/cobol_src/ACCTCTRL.cbl" line="133">

---

The program starts with the PREMIERE SECTION, which initializes the necessary data and variables. It then calls the GET-NUMBER-OF-ACCOUNTS-DB2 SECTION to retrieve the number of accounts for a given sort code from a database. The result is then displayed using the DISPLAY statement. Finally, the program exits using the EXIT statement.

```
       PREMIERE SECTION.
       P010.



           MOVE SORTCODE TO
              REQUIRED-SORT-CODE.

           PERFORM GET-NUMBER-OF-ACCOUNTS-DB2


      D    DISPLAY 'OUTPUT DATA IS='
      D       DFHCOMMAREA.


           PERFORM GET-ME-OUT-OF-HERE.
```

---

</SwmSnippet>

<SwmSnippet path="src/base/cobol_src/ACCTCTRL.cbl" line="157">

---

### GET-NUMBER-OF-ACCOUNTS-DB2

This section retrieves the number of accounts for a given sort code from a database.

```
       GET-NUMBER-OF-ACCOUNTS-DB2 SECTION.
       WCD010.

           INITIALIZE DFHCOMMAREA.


           MOVE REQUIRED-SORT-CODE TO HV-ACCOUNT-SORTCODE

           EXEC SQL
              SELECT COUNT(*)
              INTO  :HV-NUMBER-OF-ACCOUNTS
              FROM ACCOUNT
              WHERE ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE
           END-EXEC.

           IF SQLCODE = ZERO
             MOVE 'Y' TO ACCOUNT-CONTROL-SUCCESS-FLAG
             MOVE HV-NUMBER-OF-ACCOUNTS TO NUMBER-OF-ACCOUNTS
           ELSE
             MOVE 'N' TO ACCOUNT-CONTROL-SUCCESS-FLAG
             MOVE SQLCODE TO SQLCODE-DISPLAY
           END-IF.
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm AI 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa"><sup>Powered by [Swimm](https://staging.swimm.cloud/)</sup></SwmMeta>
