# Contributor's guide

## Bug Reports

Please [open an issue][new-issue].

The more detailed your report, the faster it can be resolved. Once the
bug has been resolved, the person responsible will tag the issue as
_needs confirmation_ and assign the issue back to you. Once you have
tested and confirmed that the issue is resolved, close the issue. If
you are not a member of the project, you will be asked for
confirmation and we will close it.

[new-issue]: https://github.com/tweag/capabilities-via/issues/new

## Code

1. Explain your idea and discuss your plan with members of the team.
   The best way to do this is to create an [issue][issue-tracker] or
   comment on an existing issue.
1. Prepare a git commit with your change. Don't forget to
   add [tests][tests]. Update [README.md](./README.md) if appropriate.
1. [Create a pull request][create-pull-request]. This will start the
   code review process. Enable "Allow edits from maintainers" if the
   branch is on a fork, so that reviewers can fix trivial problems
   (typos) directly. **All submissions, including submissions by
   project members, require review.**
1. You may be asked to make some changes. Continuous integration bots
   will test your change automatically on supported platforms. Once
   everything looks good, your change will be merged. The minimum
   criteria for acceptance are:

   * a patch should be a minimal and accurate answer to exactly one
     identified and agreed problem,
   * a patch that modifies a stable public API should not break existing
     applications unless there is overriding consensus on the value of
     doing this.
   * A patch must adhere to the [code style guidelines][style-guide]
     of the project.

[issue-tracker]: https://github.com/tweag/capabilities-via/issues
[tests]: ./examples
[create-pull-request]: https://help.github.com/articles/creating-a-pull-request/
[style-guide]: https://github.com/tweag/guides/blob/master/style/
