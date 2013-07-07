package com.tomykaira.uchronie.git

import org.eclipse.jgit.revwalk.RevCommit

object CommitThread {
  def fromVirtualCommits(cs: List[VirtualCommit]): CommitThread =
    new CommitThread {
      val commits: List[VirtualCommit] = cs
    }

  def fromRevCommits(cs: List[RevCommit]): CommitThread =
    new CommitThread {
      val commits: List[VirtualCommit] = cs.map(Commit.revCommitToRawCommit)
    }
}

trait CommitThread {
  val commits: List[VirtualCommit]
}
