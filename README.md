# Setup

### Postgres

```
$ sudo docker run --name postgres -e POSTGRES_PASSWORD=docker -d -p 5432:5432 -v $PWD/postgres:/var/lib/postgresql/data postgres
```

```
vagrant@ubuntu-xenial:/vagrant$ sudo docker exec -it postgres /bin/bash
root@98a238f7a488:/# psql -U postgres app
psql (11.5 (Debian 11.5-1.pgdg90+1))
Type "help" for help.

app=#
```

### MySQL

```
$ sudo docker run --name mysql -e MYSQL_DATABASE=app -e MYSQL_USER=mysql -e MYSQL_PASSWORD=mysql -e MYSQL_RANDOM_ROOT_PASSWORD=true -d -p 3306:3306 -v /tmp/mysql:/var/lib/mysql mysql
```

```
$ sudo docker exec -it mysql /bin/bash
root@f7705ddbb50a:/# mysql
```
