#!/usr/bin/env python
import requests
import os
from time import sleep
import json

class ApiNotFoundError(Exception):
    pass

class HydraEvalMonitor:
    def __init__(self, repo, job, hydra_url="https://hydra.iohk.io", jobset="Cardano"):
        self.hydraUrl = hydra_url
        self.repo = repo
        self.jobset = jobset
        self.jobPath = None
        self.evalId = None
        self.eval = None
        self.jobName = job
        self.job = self.getJob(job)
        print(f"Jobset found: \033]1339;url='{self.hydraUrl}/{self.jobPath}'\a\n")

    def getApi(self, path):
        hydra_result = requests.get(f"{self.hydraUrl}/{path}", headers={"Content-Type": "application/json"})
        if hydra_result.status_code == 200:
            return json.loads(hydra_result.text)
        else:
            raise ApiNotFoundError(f"Failed to find result for {path}")

    def getJob(self, job, retries=60, retry_time=60):
        print(f"Attempting to find hydra job {job}")
        retry_count = 0
        self.jobPath = f"jobset/{self.jobset}/{job}"
        while retry_count < retries:
            try:
                job = self.getApi(self.jobPath)
                return job
            except ApiNotFoundError as e:
                if retry_count < retries:
                    retry_count = retry_count + 1
                    print(f"Hydra job not created yet - sleeping {retry_time} seconds")
                    sleep(retry_time)
                else:
                    raise e

    def loadEvalByCommit(self, commit, retries=60, retry_time=60):
        print(f"Attempting to find eval for commit: {commit}")
        retry_count = 0
        found = False
        while not found and retry_count < retries:
            try:
                hydra_evals = self.getApi(f"{self.jobPath}/evals")
                for hydra_eval in hydra_evals["evals"]:
                    if hydra_eval["jobsetevalinputs"][self.repo]["revision"] == commit:
                        found = True
                        self.evalId = hydra_eval["id"]
                        self.eval = self.getApi(f"eval/{self.evalId}")
                if not found:
                    raise ApiNotFoundError(f"Eval not created")
            except ApiNotFoundError as e:
                print(f"Hydra eval not created yet for {commit} - sleeping {retry_time} seconds")
                retry_count = retry_count + 1
                sleep(retry_time)
                if retry_count == retries:
                    print("Retried 1 hour - exiting")
                    errormsg = self.job["errormsg"]
                    print("Errors below may be incomplete")
                    print(f"An error occurred in evaluation:\n{errormsg}")
                    raise e

    def getBuild(self, build_id):
        pass

    def checkRequiredExists(self):
        for build in self.eval["builds"]:
            hydra_build = self.getApi(f"build/{build}")
            if hydra_build["job"] == "required":
                return True
        return False

    def getErrors(self):
        try:
            return self.job["errormsg"]
        except Exception as e:
            print("An error occurred with loading eval")
            raise e


def main():
    BUILDKITE_BRANCH = os.getenv("BUILDKITE_BRANCH", None)
    BUILDKITE_COMMIT = os.getenv("BUILDKITE_COMMIT", None)
    BUILDKITE_PR = os.getenv("BUILDKITE_PULL_REQUEST", None)
    BUILDKITE_REPO = os.getenv("BUILDKITE_PIPELINE_NAME", None)

    if BUILDKITE_REPO == "iohk-monitoring-framework":
        BUILDKITE_REPO = "iohk-monitoring"

    if BUILDKITE_BRANCH == "bors/staging":
        job = f"{BUILDKITE_REPO}-bors-staging"
    elif BUILDKITE_BRANCH == "bors/trying":
        job = f"{BUILDKITE_REPO}-bors-trying"
    elif BUILDKITE_BRANCH == "master":
        job = f"{BUILDKITE_REPO}"
    elif BUILDKITE_BRANCH == "develop":
        job = f"{BUILDKITE_REPO}"
    elif BUILDKITE_PR == "false":
        print("Please open a PR for hydra to evaluate")
        raise BaseException
    else:
        job = f"{BUILDKITE_REPO}-pr-{BUILDKITE_PR}"
        print(f"PR: {BUILDKITE_PR}")
    hydraEvalMonitor = HydraEvalMonitor(job=job,repo=BUILDKITE_REPO)
    hydraEvalMonitor.loadEvalByCommit(BUILDKITE_COMMIT)
    errors = hydraEvalMonitor.getErrors()
    if hydraEvalMonitor.checkRequiredExists():
        print(f"A required jobset was found. Eval errors are ignored:\n{errors}")
    else:
        print(f"No required jobset was found. check eval errors below:\n{errors}")
        raise BaseException

main()
