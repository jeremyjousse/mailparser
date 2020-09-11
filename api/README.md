# Mailparser API

## Simple Brand CRUD

A brand is a model handling:

- name: varchar
- url: varchar

It descibes a brand which sends mailing list emails.

A sender is a model handling:

- from: varchar
- returnPath: varchar
- brandId: int (the brand identifier)

It describes an email address sending brand communications

## Roadmap

Add Google API authentication [Tutorial](https://whatthefunctional.wordpress.com/2018/07/01/google-sheets-and-haskell/) and [Code](https://github.com/WhatTheFunctional/GoogleSheetsDemo)

Persist Gmail token using [IORef](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-IORef.html#t:IORef)

## Develop

```bash
cd api
export $(cat .env | xargs)
stack repl
:load Gmail
listMessages
```
