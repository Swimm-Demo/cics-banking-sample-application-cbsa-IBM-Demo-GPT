---
title: Carbon React Front-End Overview
---
## The <SwmPath>[src/bank-application-frontend/](src/bank-application-frontend/)</SwmPath> directory

The <SwmPath>[src/bank-application-frontend/](src/bank-application-frontend/)</SwmPath> directory is the root directory for the frontend code in this repository. It contains all the source code for the frontend application, including React components, CSS styles, and other assets.

## The <SwmPath>[src/bank-application-frontend/public/](/src/bank-application-frontend/public/)</SwmPath> directory

The public directory is a special directory in the React application that contains static assets such as HTML files, images, and other files that are served by the web server. In this repository, the public directory contains the <SwmPath>[src/bank-application-frontend/public/index.html](src/bank-application-frontend/public/index.html)</SwmPath> file, which is the entry point for the frontend application.

## The <SwmPath>[src/bank-application-frontend/src/content/](src/bank-application-frontend/src/content/)</SwmPath> directory

The <SwmPath>[src/bank-application-frontend/src/content/](src/bank-application-frontend/src/content/)</SwmPath> directory contains the content for the different pages and components in the frontend application. It includes subdirectories for each page or component, such as <SwmToken path="/src/bank-application-frontend/src/App.js" pos="16:2:2" line-data="import CustomerDetailsPage from &#39;./content/CustomerDetailsPage&#39;;">`CustomerDetailsPage`</SwmToken>, <SwmToken path="src/bank-application-frontend/package.json" pos="7:2:2" line-data="  &quot;homepage&quot;: &quot;/webui-1.0/&quot;,">`homepage`</SwmToken>, and <SwmToken path="/src/bank-application-frontend/src/App.js" pos="13:2:2" line-data="import AdminPage from &#39;./content/AdminPage&#39;;">`AdminPage`</SwmToken>.

## The <SwmPath>[src/bank-application-frontend/src/components/](src/bank-application-frontend/src/components/)</SwmPath> directory

The <SwmPath>[src/bank-application-frontend/src/components/](src/bank-application-frontend/src/components/)</SwmPath> directory contains reusable React components that can be used throughout the frontend application. These components can be used to create consistent and reusable UI elements, such as buttons, forms, and navigation menus.

## The <SwmPath>[src/bank-application-frontend/src/App.js](src/bank-application-frontend/src/App.js)</SwmPath> file

The <SwmPath>[src/bank-application-frontend/src/App.js](src/bank-application-frontend/src/App.js)</SwmPath> file is the entry point for the frontend application. It contains the main App component, which is responsible for rendering the entire UI of the application.

# Running and Debugging

<SwmSnippet path="/src/bank-application-frontend/package.json" line="9">

---

## Build configuration

The build configuration is defined in the <SwmPath>[src/bank-application-frontend/package.json](src/bank-application-frontend/package.json)</SwmPath> file.

```json
  "scripts": {
    "build": "react-scripts build",
    "ci-check": "yarn format:diff",
```

---

</SwmSnippet>

To run the frontend application, run the command <SwmToken path="src/bank-application-frontend/yarn-error.log" pos="24:0:0" line-data="npm manifest: ">`npm`</SwmToken>` `<SwmToken path="src/bank-application-frontend/package.json" pos="17:2:2" line-data="    &quot;start&quot;: &quot;react-scripts start&quot;,">`start`</SwmToken> in the root directory of the project. This will start a development server and open the application in a web browser. To debug the application, you can use the developer tools in the web browser.

&nbsp;

*This is an auto-generated document by Swimm AI 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
