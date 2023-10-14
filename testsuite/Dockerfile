# This docker image is used in tests with the `docker_wrapper` driver.

FROM alire/gnat:ubuntu-lts

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

RUN pip3 install e3-testsuite

WORKDIR /testsuite
USER user
