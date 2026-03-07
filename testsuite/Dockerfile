# This docker image is used in tests with the `docker_wrapper` driver.

# Latest tested is 24.04. We use our image from the github registry.
FROM ghcr.io/alire-project/docker/gnat:ubuntu-lts

RUN useradd -m -s /bin/bash user && \
    chown user:user /home/user && \
    echo "user ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
# We need sudo permission to run `unshare`

RUN apt-get update && apt-get install -y \
    gnat \
    gprbuild \
    libcap2-bin     # for `setcap` \
    rsync \
    sudo \
    util-linux      # for `unshare`

# Use parent testsuite python packages here too, as they are potential imports
# of the drivers and helpers that may be needed in the wrapped test.
COPY ./testsuite/requirements.txt /testsuite/requirements.txt
RUN pip3 install -r /testsuite/requirements.txt \
        --break-system-packages --no-cache-dir

WORKDIR /testsuite
USER user
