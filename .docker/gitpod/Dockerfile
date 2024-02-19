# This is the Dockerfile for `algoxy:gitpod`.

# gitpod doesn't support multiple FROM statements, (or rather, you can't copy from one to another)
# so we just install everything in one go
FROM ubuntu:jammy

USER root

# apk add make font-noto-cjk ttf-freefont
RUN apt-get update && apt-get install make perl font-noto-cjk ttf-freefont sudo git curl git bash-completion -y && apt-get clean

# install tex-live
# reference from: https://www.tug.org/texlive/quickinstall.html
RUN wget https://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
RUN zcat < install-tl-unx.tar.gz | tar xf -
RUN cd install-tl-*
RUN perl ./install-tl --no-interaction # as root or with writable destination

RUN useradd -l -u 33333 -G sudo -md /home/gitpod -s /bin/bash -p gitpod gitpod \
    # passwordless sudo for users in the 'sudo' group
    && sed -i.bkp -e 's/%sudo\s\+ALL=(ALL\(:ALL\)\?)\s\+ALL/%sudo ALL=NOPASSWD:ALL/g' /etc/sudoers
USER gitpod
WORKDIR /home/gitpod

SHELL ["/bin/bash", "-c"]

# gitpod bash prompt
RUN { echo && echo "PS1='\[\033[01;32m\]\u\[\033[00m\] \[\033[01;34m\]\w\[\033[00m\]\$(__git_ps1 \" (%s)\") $ '" ; } >> .bashrc

# prepend /usr/local/texlive/YYYY/bin/PLATFORM to user's PATH,
# e.g., /usr/local/texlive/2023/bin/x86_64-linux

ENV PATH="/home/gitpod/.local/bin:/usr/local/texlive/2023/bin/x86_64-linux:${PATH}"

# fix the infoview when the container is used on gitpod:
ENV VSCODE_API_VERSION="1.50.0"

# ssh to github once to bypass the unknown fingerprint warning
RUN ssh -o StrictHostKeyChecking=no github.com || true

# run sudo once to suppress usage info
RUN sudo echo finished