# This docker image is used in tests with the `docker_wrapper` driver.

FROM alire/gnat:ubuntu-lts

RUN useradd -m -s /bin/bash user && \
    chown user:user /home/user

RUN pip3 install e3-testsuite

WORKDIR /testsuite
USER user
