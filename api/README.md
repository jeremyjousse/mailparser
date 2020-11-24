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

## TODO

Il y aura 4 lignes de code qui te feront peur / que tu ne pourras peut-être pas comprendre complètement, désolé 😀
Si tu te sens l'âme aventurière, tu peux regarder comment par exemple https://github.com/dktunited/workshop-analytics fait en même temps du Servant (donc Handler) et de la DB (eux ils utilisent MongoDB avec la lib Mongo alors que tu utilises PostgreSQL avec Persistent, mais le besoin est très similaire, véhiculer "une pool DB" pour envoyer les requêtes)

La ligne qui va te faire peur : https://github.com/dktunited/workshop-analytics-haskell/blob/develop/src/Application.hs#L37
En gros, au lieu d'écrire ensuite le code métier dans un contexte Handler (donc qui ne peut faire que du Servant), ils écrivent leur application dans un context custom AppContext (défini ici, mais là c'est un poil plus spécifique Mongo : https://github.com/dktunited/workshop-analytics/blob/develop/src/Controller.hs#L29 )
Et en gros, il faut ensuite expliquer à Servant comment convertir un AppContext en Handler, c'est le rôle de la fonction effrayante hoistServerWithContext

Pour toi au lieu de type AppContext = Action Handler ce sera plutôt un truc du genre type AppContext = ReaderT SqlBackend Handler
Mais j'ai pas testé, il faut sûrement mettre quelques coups de tournevis

https://github.com/parsonsmatt/servant-persistent/blob/master/src/Config.hs

## To look

https://hackage.haskell.org/package/gogol-gmail-0.5.0/docs/Network-Google-Gmail-Types.html
