# Google Authentication workflow

## Request code

`https://accounts.google.com/o/oauth2/v2/auth?client_id={client_id}&redirect_uri=http://localhost&response_type=code&scope=https://mail.google.com/&access_type=offline`

## Request fist token and refresh token

`https://oauth2.googleapis.com/token` whith data-urlencode `code={code}&client_id={client_id}&client_secret={client_secret}&redirect_uri=https%3A//oauth2.example.com/code&grant_type=authorization_code`

## Refresh token

`https://oauth2.googleapis.com/token` whith data-urlencode `code={code}&client_id={client_id}&client_secret={client_secret}&redirect_uri=https%3A//oauth2.example.com/code&grant_type=refresh_token&refresh_token={refresh_token}`
