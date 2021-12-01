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
-- Module      : Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about CloudWatch Logs for the build project.
--
-- /See:/ 'newAwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' smart constructor.
data AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails = AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails'
  { -- | The current status of the logs in CloudWatch Logs for a build project.
    status :: Prelude.Maybe Prelude.Text,
    -- | The group name of the logs in CloudWatch Logs.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The prefix of the stream name of the CloudWatch Logs.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_status' - The current status of the logs in CloudWatch Logs for a build project.
--
-- 'groupName', 'awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_groupName' - The group name of the logs in CloudWatch Logs.
--
-- 'streamName', 'awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_streamName' - The prefix of the stream name of the CloudWatch Logs.
newAwsCodeBuildProjectLogsConfigCloudWatchLogsDetails ::
  AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
newAwsCodeBuildProjectLogsConfigCloudWatchLogsDetails =
  AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails'
    { status =
        Prelude.Nothing,
      groupName =
        Prelude.Nothing,
      streamName =
        Prelude.Nothing
    }

-- | The current status of the logs in CloudWatch Logs for a build project.
awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_status :: Lens.Lens' AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_status = Lens.lens (\AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {status} -> status) (\s@AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {} a -> s {status = a} :: AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails)

-- | The group name of the logs in CloudWatch Logs.
awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_groupName :: Lens.Lens' AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_groupName = Lens.lens (\AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {groupName} -> groupName) (\s@AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {} a -> s {groupName = a} :: AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails)

-- | The prefix of the stream name of the CloudWatch Logs.
awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_streamName :: Lens.Lens' AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_streamName = Lens.lens (\AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {streamName} -> streamName) (\s@AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {} a -> s {streamName = a} :: AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails)

instance
  Core.FromJSON
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
  where
  parseJSON =
    Core.withObject
      "AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails"
      ( \x ->
          AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails'
            Prelude.<$> (x Core..:? "Status")
              Prelude.<*> (x Core..:? "GroupName")
              Prelude.<*> (x Core..:? "StreamName")
      )

instance
  Prelude.Hashable
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
  where
  hashWithSalt
    salt'
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {..} =
      salt' `Prelude.hashWithSalt` streamName
        `Prelude.hashWithSalt` groupName
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
  where
  rnf
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {..} =
      Prelude.rnf status
        `Prelude.seq` Prelude.rnf streamName
        `Prelude.seq` Prelude.rnf groupName

instance
  Core.ToJSON
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
  where
  toJSON
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Status" Core..=) Prelude.<$> status,
              ("GroupName" Core..=) Prelude.<$> groupName,
              ("StreamName" Core..=) Prelude.<$> streamName
            ]
        )
