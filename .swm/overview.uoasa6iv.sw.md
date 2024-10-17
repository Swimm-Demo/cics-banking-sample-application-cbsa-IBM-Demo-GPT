---
title: Overview
---
The repository provides a sample application that simulates the operation of a bank from the perspective of a bank teller. It integrates various technologies like CICS, COBOL, Db2, Java, and Spring Boot, and offers multiple interfaces including COBOL (BMS), Carbon React UI, Customer Services, and Payment interfaces. It serves as a teaching aid, a tool for testing CICS interactions, and a foundation for application modernization discussions.

## Main Components

### Core Banking

- <SwmLink doc-title="Funds transfer handler xfrfun program">[Funds transfer handler xfrfun program](.swm/funds-transfer-handler-xfrfun-program.01mixfl6.sw.md)</SwmLink>
- <SwmLink doc-title="Customer control management custctrl program">[Customer control management custctrl program](.swm/customer-control-management-custctrl-program.kucnswv3.sw.md)</SwmLink>
- <SwmLink doc-title="Customer and account deletion delcus program">[Customer and account deletion delcus program](.swm/customer-and-account-deletion-delcus-program.3a8oyrhw.sw.md)</SwmLink>
- <SwmLink doc-title="Processing application abends abndproc program">[Processing application abends abndproc program](.swm/processing-application-abends-abndproc-program.6ebca89q.sw.md)</SwmLink>
- <SwmLink doc-title="Updating account details updacc program">[Updating account details updacc program](.swm/updating-account-details-updacc-program.eid2nuob.sw.md)</SwmLink>
- <SwmLink doc-title="Account control operations acctctrl program">[Account control operations acctctrl program](.swm/account-control-operations-acctctrl-program.fprflqzn.sw.md)</SwmLink>
- <SwmLink doc-title="Credit scoring simulation crdtagy5 program">[Credit scoring simulation crdtagy5 program](.swm/credit-scoring-simulation-crdtagy5-program.qq5u4w10.sw.md)</SwmLink>
- <SwmLink doc-title="Credit scoring simulation crdtagy4 program">[Credit scoring simulation crdtagy4 program](.swm/credit-scoring-simulation-crdtagy4-program.e2rb8pwj.sw.md)</SwmLink>
- <SwmLink doc-title="Credit scoring simulation crdtagy1 program">[Credit scoring simulation crdtagy1 program](.swm/credit-scoring-simulation-crdtagy1-program.42o6dcca.sw.md)</SwmLink>
- <SwmLink doc-title="Credit scoring simulation crdtagy3 program">[Credit scoring simulation crdtagy3 program](.swm/credit-scoring-simulation-crdtagy3-program.t3jqcrhs.sw.md)</SwmLink>
- <SwmLink doc-title="Setting sort code getscode program">[Setting sort code getscode program](.swm/setting-sort-code-getscode-program.8i0zepe6.sw.md)</SwmLink>
- <SwmLink doc-title="Setting company name getcompy program">[Setting company name getcompy program](.swm/setting-company-name-getcompy-program.u2m8ihby.sw.md)</SwmLink>
- <SwmLink doc-title="Updating customer details updcust program">[Updating customer details updcust program](.swm/updating-customer-details-updcust-program.ouvyqf87.sw.md)</SwmLink>
- <SwmLink doc-title="Deleting account records delacc program">[Deleting account records delacc program](.swm/deleting-account-records-delacc-program.6hsxsy8z.sw.md)</SwmLink>
- <SwmLink doc-title="Cash transaction processing dbcrfun program">[Cash transaction processing dbcrfun program](.swm/cash-transaction-processing-dbcrfun-program.c4amzafh.sw.md)</SwmLink>
- **Bnk1cac**
  - <SwmLink doc-title="Creating a new bank account bnk1cac program">[Creating a new bank account bnk1cac program](.swm/creating-a-new-bank-account-bnk1cac-program.qoa1ecz9.sw.md)</SwmLink>
- **Bnk1dac**
  - <SwmLink doc-title="Account management bnk1dac program">[Account management bnk1dac program](.swm/account-management-bnk1dac-program.8hncpa9b.sw.md)</SwmLink>
- **Bnk1tfn**
  - <SwmLink doc-title="Fund transfer processing bnk1tfn program">[Fund transfer processing bnk1tfn program](.swm/fund-transfer-processing-bnk1tfn-program.ypyb5xz4.sw.md)</SwmLink>
- **Inqcust**
  - <SwmLink doc-title="Customer information retrieval inqcust program">[Customer information retrieval inqcust program](.swm/customer-information-retrieval-inqcust-program.t5be85z8.sw.md)</SwmLink>
- **Bnk1cra**
  - <SwmLink doc-title="Creditdebit operations bnk1cra program">[Creditdebit operations bnk1cra program](.swm/creditdebit-operations-bnk1cra-program.lmwxrdox.sw.md)</SwmLink>
- **Inqacccu**
  - <SwmLink doc-title="Customer account inquiry inqacccu program">[Customer account inquiry inqacccu program](.swm/customer-account-inquiry-inqacccu-program.dc0ff1qo.sw.md)</SwmLink>
- **Bnk1uac**
  - <SwmLink doc-title="Updating account information bnk1uac program">[Updating account information bnk1uac program](.swm/updating-account-information-bnk1uac-program.w44f7rw5.sw.md)</SwmLink>
- **Bankdata**
  - <SwmLink doc-title="Data initialization bankdata program">[Data initialization bankdata program](.swm/data-initialization-bankdata-program.pon0bmgh.sw.md)</SwmLink>
- **Crecust**
  - <SwmLink doc-title="Customer information processing crecust program">[Customer information processing crecust program](.swm/customer-information-processing-crecust-program.t2sxtzx9.sw.md)</SwmLink>
- **Bnk1dcs**
  - <SwmLink doc-title="Bnk1dcs program">[Bnk1dcs program](.swm/bnk1dcs-program.885i98g4.sw.md)</SwmLink>
- **Bnk1ccs**
  - <SwmLink doc-title="Creating a new customer bnk1ccs program">[Creating a new customer bnk1ccs program](.swm/creating-a-new-customer-bnk1ccs-program.wjx4rqn6.sw.md)</SwmLink>
- **Bnkmenu**
  - <SwmLink doc-title="Bank menu processing bnkmenu program">[Bank menu processing bnkmenu program](.swm/bank-menu-processing-bnkmenu-program.r6w9zlnc.sw.md)</SwmLink>
- **Bnk1cca**
  - <SwmLink doc-title="Listing customer accounts bnk1cca program">[Listing customer accounts bnk1cca program](.swm/listing-customer-accounts-bnk1cca-program.spk0iv3a.sw.md)</SwmLink>
- **Creacc**
  - <SwmLink doc-title="Creating new bank accounts creacc program">[Creating new bank accounts creacc program](.swm/creating-new-bank-accounts-creacc-program.ekmxwud7.sw.md)</SwmLink>
- **Inqacc**
  - <SwmLink doc-title="Account information retrieval inqacc program">[Account information retrieval inqacc program](.swm/account-information-retrieval-inqacc-program.zopanwh0.sw.md)</SwmLink>

### Web UI

The Web UI refers to the browser interface built on the Carbon React system, which is part of the WebSphere Liberty Profile application. It provides a user-friendly interface for interacting with the underlying Java application running inside the WebSphere Liberty Profile JVM server inside CICS.

- <SwmLink doc-title="Web ui overview">[Web ui overview](.swm/web-ui-overview.hhn6l4t9.sw.md)</SwmLink>
- **Datainterfaces**
  - <SwmLink doc-title="Introduction to data interfaces">[Introduction to data interfaces](.swm/introduction-to-data-interfaces.bob5pch6.sw.md)</SwmLink>
  - **Proctran**
    - <SwmLink doc-title="Basic concepts of processed transaction data">[Basic concepts of processed transaction data](.swm/basic-concepts-of-processed-transaction-data.tqyxqauw.sw.md)</SwmLink>
- **Webui**
  - <SwmLink doc-title="Overview of web ui">[Overview of web ui](.swm/overview-of-web-ui.ywlv4bor.sw.md)</SwmLink>
- **Api**
  - **Customer resource**
    - <SwmLink doc-title="Customer resource api overview">[Customer resource api overview](.swm/customer-resource-api-overview.pvpw5jgu.sw.md)</SwmLink>
    - **Flows**
      - <SwmLink doc-title="Adding a new customer flow">[Adding a new customer flow](.swm/adding-a-new-customer-flow.mfok0u9y.sw.md)</SwmLink>
      - <SwmLink doc-title="Customer deletion flow">[Customer deletion flow](.swm/customer-deletion-flow.2ko00w8z.sw.md)</SwmLink>
      - <SwmLink doc-title="Customer data update flow">[Customer data update flow](.swm/customer-data-update-flow.yvvjw1f8.sw.md)</SwmLink>
  - **Accounts resource**
    - <SwmLink doc-title="Overview of accounts api resource">[Overview of accounts api resource](.swm/overview-of-accounts-api-resource.t23z6nmf.sw.md)</SwmLink>
  - **Flows**
    - <SwmLink doc-title="Account deletion flow">[Account deletion flow](.swm/account-deletion-flow.04bjcshx.sw.md)</SwmLink>
    - <SwmLink doc-title="Handling debit and credit transactions">[Handling debit and credit transactions](.swm/handling-debit-and-credit-transactions.7z83mwhf.sw.md)</SwmLink>
    - <SwmLink doc-title="Retrieving customer accounts flow">[Retrieving customer accounts flow](.swm/retrieving-customer-accounts-flow.cvqkt20v.sw.md)</SwmLink>
    - <SwmLink doc-title="New account creation flow">[New account creation flow](.swm/new-account-creation-flow.8n1r6www.sw.md)</SwmLink>
    - <SwmLink doc-title="Updating account details flow">[Updating account details flow](.swm/updating-account-details-flow.ili57ayj.sw.md)</SwmLink>
    - <SwmLink doc-title="Account filtering and counting flow">[Account filtering and counting flow](.swm/account-filtering-and-counting-flow.ryscof8m.sw.md)</SwmLink>
    - <SwmLink doc-title="Retrieving account data flow">[Retrieving account data flow](.swm/retrieving-account-data-flow.ksf8d87t.sw.md)</SwmLink>
    - <SwmLink doc-title="Fund transfer process">[Fund transfer process](.swm/fund-transfer-process.yg11wd78.sw.md)</SwmLink>
    - <SwmLink doc-title="Transaction data retrieval and processing flow">[Transaction data retrieval and processing flow](.swm/transaction-data-retrieval-and-processing-flow.l0j8exmr.sw.md)</SwmLink>
    - <SwmLink doc-title="Account deletion process">[Account deletion process](.swm/account-deletion-process.foi5c807.sw.md)</SwmLink>
    - <SwmLink doc-title="New account creation flow">[New account creation flow](.swm/new-account-creation-flow.cff6p4ru.sw.md)</SwmLink>
    - <SwmLink doc-title="Customer creation process">[Customer creation process](.swm/customer-creation-process.neyf8ajf.sw.md)</SwmLink>
  - **Classes**
    - <SwmLink doc-title="The hbankdataaccess class">[The hbankdataaccess class](.swm/the-hbankdataaccess-class.5tpl4.sw.md)</SwmLink>
- **Web**
  - **Account**
    - <SwmLink doc-title="Overview of the account class in web module">[Overview of the account class in web module](.swm/overview-of-the-account-class-in-web-module.qkzudy8i.sw.md)</SwmLink>
    - **Flows**
      - <SwmLink doc-title="Updating account balance">[Updating account balance](.swm/updating-account-balance.veauq6ho.sw.md)</SwmLink>
  - **Processed transaction**
    - <SwmLink doc-title="Processed transactions in web applications">[Processed transactions in web applications](.swm/processed-transactions-in-web-applications.fq7fiaxb.sw.md)</SwmLink>
- **Build tools**
  - <SwmLink doc-title="Building the webui with maven">[Building the webui with maven](.swm/building-the-webui-with-maven.nxhl6o9c.sw.md)</SwmLink>
- **Flows**
  - <SwmLink doc-title="Customer count retrieval flow">[Customer count retrieval flow](.swm/customer-count-retrieval-flow.b98w4tc0.sw.md)</SwmLink>
  - <SwmLink doc-title="Retrieving customer data flow">[Retrieving customer data flow](.swm/retrieving-customer-data-flow.jqozr6a2.sw.md)</SwmLink>

### Customer Services API

- **Controllers**
  - <SwmLink doc-title="Exploring customer services controllers">[Exploring customer services controllers](.swm/exploring-customer-services-controllers.3r9yclmy.sw.md)</SwmLink>
- **Jsonclasses**
  - <SwmLink doc-title="Introduction to jsonclasses in customer services api">[Introduction to jsonclasses in customer services api](.swm/introduction-to-jsonclasses-in-customer-services-api.hxmo71fc.sw.md)</SwmLink>
- **Build tools**
  - <SwmLink doc-title="Building the z os connect customer services interface with maven">[Building the z os connect customer services interface with maven](.swm/building-the-z-os-connect-customer-services-interface-with-maven.fjgzn541.sw.md)</SwmLink>

### Payment API

The Payment API is a Spring Boot application that facilitates backend banking operations through RESTful API calls to a zOS Connect server, which then routes requests to a CICS region. It allows companies to process payments and issue refunds from accounts at CBSA Bank, providing a straightforward interface for these transactions.

- <SwmLink doc-title="Payment api overview">[Payment api overview](.swm/payment-api-overview.1l8f4d35.sw.md)</SwmLink>
- **Build tools**
  - <SwmLink doc-title="Building the payment interface with maven">[Building the payment interface with maven](.swm/building-the-payment-interface-with-maven.ixfmtaan.sw.md)</SwmLink>

### Frontend Application

- **Customer details page**
  - <SwmLink doc-title="Exploring customer details page">[Exploring customer details page](.swm/exploring-customer-details-page.1plgzvfn.sw.md)</SwmLink>
- **Account details page**
  - <SwmLink doc-title="Basic concepts of the account details page">[Basic concepts of the account details page](.swm/basic-concepts-of-the-account-details-page.bqbjkpd6.sw.md)</SwmLink>
- **Customer delete page**
  - <SwmLink doc-title="Getting started with customer deletion page">[Getting started with customer deletion page](.swm/getting-started-with-customer-deletion-page.kv07vpmr.sw.md)</SwmLink>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
