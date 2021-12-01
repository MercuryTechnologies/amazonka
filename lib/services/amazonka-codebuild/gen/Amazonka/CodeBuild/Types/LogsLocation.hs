{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeBuild.Types.LogsLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.LogsLocation where

import Amazonka.CodeBuild.Types.CloudWatchLogsConfig
import Amazonka.CodeBuild.Types.S3LogsConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about build logs in CloudWatch Logs.
--
-- /See:/ 'newLogsLocation' smart constructor.
data LogsLocation = LogsLocation'
  { -- | The URL to an individual build log in CloudWatch Logs.
    deepLink :: Prelude.Maybe Prelude.Text,
    -- | Information about S3 logs for a build project.
    s3Logs :: Prelude.Maybe S3LogsConfig,
    -- | Information about CloudWatch Logs for a build project.
    cloudWatchLogs :: Prelude.Maybe CloudWatchLogsConfig,
    -- | The URL to a build log in an S3 bucket.
    s3DeepLink :: Prelude.Maybe Prelude.Text,
    -- | The ARN of S3 logs for a build project. Its format is
    -- @arn:${Partition}:s3:::${BucketName}\/${ObjectName}@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3>.
    s3LogsArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of CloudWatch Logs for a build project. Its format is
    -- @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by CloudWatch Logs>.
    cloudWatchLogsArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the CloudWatch Logs group for the build logs.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the CloudWatch Logs stream for the build logs.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogsLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deepLink', 'logsLocation_deepLink' - The URL to an individual build log in CloudWatch Logs.
--
-- 's3Logs', 'logsLocation_s3Logs' - Information about S3 logs for a build project.
--
-- 'cloudWatchLogs', 'logsLocation_cloudWatchLogs' - Information about CloudWatch Logs for a build project.
--
-- 's3DeepLink', 'logsLocation_s3DeepLink' - The URL to a build log in an S3 bucket.
--
-- 's3LogsArn', 'logsLocation_s3LogsArn' - The ARN of S3 logs for a build project. Its format is
-- @arn:${Partition}:s3:::${BucketName}\/${ObjectName}@. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3>.
--
-- 'cloudWatchLogsArn', 'logsLocation_cloudWatchLogsArn' - The ARN of CloudWatch Logs for a build project. Its format is
-- @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@.
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by CloudWatch Logs>.
--
-- 'groupName', 'logsLocation_groupName' - The name of the CloudWatch Logs group for the build logs.
--
-- 'streamName', 'logsLocation_streamName' - The name of the CloudWatch Logs stream for the build logs.
newLogsLocation ::
  LogsLocation
newLogsLocation =
  LogsLocation'
    { deepLink = Prelude.Nothing,
      s3Logs = Prelude.Nothing,
      cloudWatchLogs = Prelude.Nothing,
      s3DeepLink = Prelude.Nothing,
      s3LogsArn = Prelude.Nothing,
      cloudWatchLogsArn = Prelude.Nothing,
      groupName = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | The URL to an individual build log in CloudWatch Logs.
logsLocation_deepLink :: Lens.Lens' LogsLocation (Prelude.Maybe Prelude.Text)
logsLocation_deepLink = Lens.lens (\LogsLocation' {deepLink} -> deepLink) (\s@LogsLocation' {} a -> s {deepLink = a} :: LogsLocation)

-- | Information about S3 logs for a build project.
logsLocation_s3Logs :: Lens.Lens' LogsLocation (Prelude.Maybe S3LogsConfig)
logsLocation_s3Logs = Lens.lens (\LogsLocation' {s3Logs} -> s3Logs) (\s@LogsLocation' {} a -> s {s3Logs = a} :: LogsLocation)

-- | Information about CloudWatch Logs for a build project.
logsLocation_cloudWatchLogs :: Lens.Lens' LogsLocation (Prelude.Maybe CloudWatchLogsConfig)
logsLocation_cloudWatchLogs = Lens.lens (\LogsLocation' {cloudWatchLogs} -> cloudWatchLogs) (\s@LogsLocation' {} a -> s {cloudWatchLogs = a} :: LogsLocation)

-- | The URL to a build log in an S3 bucket.
logsLocation_s3DeepLink :: Lens.Lens' LogsLocation (Prelude.Maybe Prelude.Text)
logsLocation_s3DeepLink = Lens.lens (\LogsLocation' {s3DeepLink} -> s3DeepLink) (\s@LogsLocation' {} a -> s {s3DeepLink = a} :: LogsLocation)

-- | The ARN of S3 logs for a build project. Its format is
-- @arn:${Partition}:s3:::${BucketName}\/${ObjectName}@. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3>.
logsLocation_s3LogsArn :: Lens.Lens' LogsLocation (Prelude.Maybe Prelude.Text)
logsLocation_s3LogsArn = Lens.lens (\LogsLocation' {s3LogsArn} -> s3LogsArn) (\s@LogsLocation' {} a -> s {s3LogsArn = a} :: LogsLocation)

-- | The ARN of CloudWatch Logs for a build project. Its format is
-- @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@.
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by CloudWatch Logs>.
logsLocation_cloudWatchLogsArn :: Lens.Lens' LogsLocation (Prelude.Maybe Prelude.Text)
logsLocation_cloudWatchLogsArn = Lens.lens (\LogsLocation' {cloudWatchLogsArn} -> cloudWatchLogsArn) (\s@LogsLocation' {} a -> s {cloudWatchLogsArn = a} :: LogsLocation)

-- | The name of the CloudWatch Logs group for the build logs.
logsLocation_groupName :: Lens.Lens' LogsLocation (Prelude.Maybe Prelude.Text)
logsLocation_groupName = Lens.lens (\LogsLocation' {groupName} -> groupName) (\s@LogsLocation' {} a -> s {groupName = a} :: LogsLocation)

-- | The name of the CloudWatch Logs stream for the build logs.
logsLocation_streamName :: Lens.Lens' LogsLocation (Prelude.Maybe Prelude.Text)
logsLocation_streamName = Lens.lens (\LogsLocation' {streamName} -> streamName) (\s@LogsLocation' {} a -> s {streamName = a} :: LogsLocation)

instance Core.FromJSON LogsLocation where
  parseJSON =
    Core.withObject
      "LogsLocation"
      ( \x ->
          LogsLocation'
            Prelude.<$> (x Core..:? "deepLink")
            Prelude.<*> (x Core..:? "s3Logs")
            Prelude.<*> (x Core..:? "cloudWatchLogs")
            Prelude.<*> (x Core..:? "s3DeepLink")
            Prelude.<*> (x Core..:? "s3LogsArn")
            Prelude.<*> (x Core..:? "cloudWatchLogsArn")
            Prelude.<*> (x Core..:? "groupName")
            Prelude.<*> (x Core..:? "streamName")
      )

instance Prelude.Hashable LogsLocation where
  hashWithSalt salt' LogsLocation' {..} =
    salt' `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` cloudWatchLogsArn
      `Prelude.hashWithSalt` s3LogsArn
      `Prelude.hashWithSalt` s3DeepLink
      `Prelude.hashWithSalt` cloudWatchLogs
      `Prelude.hashWithSalt` s3Logs
      `Prelude.hashWithSalt` deepLink

instance Prelude.NFData LogsLocation where
  rnf LogsLocation' {..} =
    Prelude.rnf deepLink
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf cloudWatchLogsArn
      `Prelude.seq` Prelude.rnf s3LogsArn
      `Prelude.seq` Prelude.rnf s3DeepLink
      `Prelude.seq` Prelude.rnf cloudWatchLogs
      `Prelude.seq` Prelude.rnf s3Logs
