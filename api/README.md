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

Read Payload Headers, extract the attribute named From, email address (stored between <>) from extract his value .
Then store link the communication to this sender.

```Json
{
    "name": "From",
    "value": "IKEA Family <ikea@news.email.ikea.fr>"
}
```

or

```Json
{
    "name": "From",
    "value": "\"Dockers®\" <dockers@e.dockers.com>"
},
```

X-SFMC-Stack

Read Payload Parts, check for each part if we have `"mimeType": "text/plain"` and `"mimeType": "text/html"` and even an other mimeType. Concatenate parts ordered by txt,html,other.

Add a provider field to sender table and store `nslookup -q=MX ${domain_name}` response `e.dockers.com mail exchanger = 10 imh.rsys2.net.`

## To look

https://hackage.haskell.org/package/gogol-gmail-0.5.0/docs/Network-Google-Gmail-Types.html

https://github.com/parsonsmatt/split-persistent/tree/master/src
