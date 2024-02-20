# This is the Dockerfile for `algoxy:gitpod`.

FROM ghcr.io/xu-cheng/texlive-full:latest

USER root

# Add additional pkgs to https://github.com/xu-cheng/latex-docker/blob/master/setup.sh

RUN apk add \
       font-noto-cjk ttf-freefont \
       bash-completion sudo \
    rm -rf /tmp/*

RUN useradd -l -u 33333 -G sudo -md /home/gitpod -s /bin/bash -p gitpod gitpod \
    # passwordless sudo for users in the 'sudo' group
    && sed -i.bkp -e 's/%sudo\s\+ALL=(ALL\(:ALL\)\?)\s\+ALL/%sudo ALL=NOPASSWD:ALL/g' /etc/sudoers

USER gitpod
WORKDIR /home/gitpod

SHELL ["/bin/bash", "-c"]

# gitpod bash prompt
RUN { echo && echo "PS1='\[\033[01;32m\]\u\[\033[00m\] \[\033[01;34m\]\w\[\033[00m\]\$(__git_ps1 \" (%s)\") $ '" ; } >> .bashrc

# texlive path should be /usr/local/texlive/YYYY/bin/PLATFORM to user's PATH,
# e.g., /usr/local/texlive/2023/bin/x86_64-linux

ENV PATH="/home/gitpod/.local/bin:${PATH}"

# fix the infoview when the container is used on gitpod:
ENV VSCODE_API_VERSION="1.50.0"

# ssh to github once to bypass the unknown fingerprint warning
RUN ssh -o StrictHostKeyChecking=no github.com || true

# run sudo once to suppress usage info
RUN sudo echo finished