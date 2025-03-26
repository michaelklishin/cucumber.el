FROM ruby:3.2-slim
RUN apt-get update -qq && apt-get install -y emacs-nox

# install docker
RUN apt-get install -y apt-transport-https ca-certificates curl gnupg2 software-properties-common
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add -
RUN add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian $(lsb_release -cs) stable"
RUN apt-get update && apt-get install -y docker-ce

ENV APP_HOME /myapp
RUN mkdir $APP_HOME
WORKDIR $APP_HOME

ENV BUNDLE_GEMFILE=$APP_HOME/Gemfile \
  BUNDLE_JOBS=2 \
  BUNDLE_PATH=/bundle
