# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.209.6/containers/ubuntu/.devcontainer/base.Dockerfile

# [Choice] Ubuntu version (use hirsuite or bionic on local arm64/Apple Silicon): hirsute, focal, bionic
ARG VARIANT="hirsute"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

ENV PATH=/home/vscode/gnat/bin:$PATH:/home/vscode/alire/bin:/root/.cargo/bin \
    GPR_PROJECT_PATH=/home/vscode/adalib/share/gpr

RUN sudo apt-get update \
    && sudo apt-get install -y \
        libx11-dev \
        libx11-xcb1 \
        libfontconfig \
        dbus \
        unzip \
        python3 python3-pip \
    && wget -q https://community.download.adacore.com/v1/f3a99d283f7b3d07293b2e1d07de00e31e332325?filename=gnat-2021-20210519-x86_64-linux-bin \
    -O /tmp/gnat-2021-20210519-x86_64-linux-bin \
    && wget -q https://raw.githubusercontent.com/alire-project/alire/master/scripts/gnat_install.qs \
    -O /tmp/gnat_install.qs \
    && chmod +x /tmp/gnat-2021-20210519-x86_64-linux-bin \
    && /tmp/gnat-2021-20210519-x86_64-linux-bin\
    --platform minimal --script /tmp/gnat_install.qs InstallPrefix=/home/vscode/gnat \
    && ls /home/vscode/gnat/bin \
    && gprinstall --uninstall gpr \
    && gprinstall --uninstall aunit \
    && gprinstall --uninstall aws \
    && gprinstall --uninstall gnatcoll \
    && sh -c "rm -rvf /opt/gnat/maintenancetool*" \
    && rm -rf /home/vscode/gnat/share/doc \
    && rm -rf /home/vscode/gnat/share/examples \
    && rm -rf /home/vscode/gnat/share/gps \
    && rm -rf /home/vscode/gnat/share/man \
    && find /home/vscode/gnat/ -type d -empty -delete \
    && rm -rf /tmp/gnat-2021-20210519-x86_64-linux-bin \
    && sudo apt-get purge -y --auto-remove libx11-xcb1 \
    && sudo apt-get clean \
    && sudo rm -rf /var/lib/apt/lists/*

RUN wget -q https://github.com/alire-project/alire/releases/download/v1.1.2/alr-1.1.2-bin-x86_64-linux.zip \
    -O /tmp/alr-1.1.2-bin-x86_64-linux.zip \
    && mkdir -p /home/vscode/alire \
    && unzip /tmp/alr-1.1.2-bin-x86_64-linux.zip -d /home/vscode/alire


# [Optional] Uncomment this section to install additional OS packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends <your-package-list-here>


