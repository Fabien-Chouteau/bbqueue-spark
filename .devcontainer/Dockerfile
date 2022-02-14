# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.209.6/containers/ubuntu/.devcontainer/base.Dockerfile

# [Choice] Ubuntu version (use hirsuite or bionic on local arm64/Apple Silicon): hirsute, focal, bionic
ARG VARIANT="hirsute"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

COPY scripts/gnat_install.qs /tmp/

RUN sudo apt-get update \
    && sudo apt-get install -y \
        libx11-xcb1 \
    && wget -q https://community.download.adacore.com/v1/f3a99d283f7b3d07293b2e1d07de00e31e332325?filename=gnat-2021-20210519-x86_64-linux-bin -O gnat-2021-20210519-x86_64-linux-bin \
    --output /tmp/gnat-2021-20210519-x86_64-linux-bin \
    && chmod +x /tmp/gnat-2021-20210519-x86_64-linux-bin \
    && /tmp/gnat-2021-20210519-x86_64-linux-bin\
    --platform minimal --script /tmp/gnat_install.qs InstallPrefix=$HOME/gnat \
    && gprinstall --uninstall gpr \
    && gprinstall --uninstall aunit \
    && gprinstall --uninstall aws \
    && gprinstall --uninstall gnatcoll \
    && sh -c "rm -rvf /opt/gnat/maintenancetool*" \
    && rm -rf $HOME/gnat/share/doc \
    && rm -rf $HOME/gnat/share/examples \
    && rm -rf $HOME/gnat/share/gps \
    && rm -rf $HOME/gnat/share/man \
    && find $HOME/gnat/ -type d -empty -delete \
    && rm -rf /tmp/gnat-2021-20210519-x86_64-linux-bin \
    && sudo apt-get purge -y --auto-remove libx11-xcb1 \
    && sudo apt-get clean \
    && sudo rm -rf /var/lib/apt/lists/*

# [Optional] Uncomment this section to install additional OS packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends <your-package-list-here>


