FROM ruby:3.2-slim

RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
    build-essential git emacs-nox org-mode libyaml-dev

WORKDIR /app

# copy the Gemfiles first to avoid reinstall on unrelated changes
COPY Gemfile Gemfile.lock ./
RUN bundle install

# copy the rest now
COPY . ./

CMD ["make", "test"]
