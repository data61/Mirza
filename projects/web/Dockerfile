FROM nginx:alpine

COPY deploy/etc/nginx/nginx.conf /etc/nginx/nginx.conf
COPY deploy/etc/nginx/conf.d/default.conf /etc/nginx/conf.d/default.conf

ADD deploy.tar /usr/share/nginx/html
