# Secure Custom Web Application Logins

In addition to REST applications, InterSystems products support CSP applications and Zen applications; Zen applications
are legacy only. When creating custom login pages for CSP and Zen applications, it is important that you follow recommended protocols. These protocols provide greater security and minimize incompatibilities on upgrades to new products or versions.

## 1 About Creating a Custom Zen Login Page

When creating a custom Zen login page, use the <loginForm> component as described in Controlling Access to Applications in Developing Zen Applications.

Important: When creating a custom login page, you must use the <loginForm> component. Other approaches for

creating login pages in Zen applications can cause problems of various kinds.

If you have written custom login pages that do not use the <loginForm> component and you apply any changes from InterSystems that upgrade or secure your instance, your login pages may fail without error messages. For example, users may attempt to log in with valid usernames and passwords, but their logins will fail without any visible cause. This situation may indicate that you need to change your custom login to use the required approach.

## 2 See Also

- Introduction to CSP-based Web Applications
