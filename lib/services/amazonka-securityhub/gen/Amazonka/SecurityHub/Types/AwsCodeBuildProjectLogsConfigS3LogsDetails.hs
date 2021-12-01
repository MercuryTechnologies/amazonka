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
-- Module      : Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigS3LogsDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigS3LogsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about logs built to an S3 bucket for a build project.
--
-- /See:/ 'newAwsCodeBuildProjectLogsConfigS3LogsDetails' smart constructor.
data AwsCodeBuildProjectLogsConfigS3LogsDetails = AwsCodeBuildProjectLogsConfigS3LogsDetails'
  { -- | The current status of the S3 build logs.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the S3 bucket and the path prefix for S3 logs.
    location :: Prelude.Maybe Prelude.Text,
    -- | Whether to disable encryption of the S3 build log output.
    encryptionDisabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCodeBuildProjectLogsConfigS3LogsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsCodeBuildProjectLogsConfigS3LogsDetails_status' - The current status of the S3 build logs.
--
-- 'location', 'awsCodeBuildProjectLogsConfigS3LogsDetails_location' - The ARN of the S3 bucket and the path prefix for S3 logs.
--
-- 'encryptionDisabled', 'awsCodeBuildProjectLogsConfigS3LogsDetails_encryptionDisabled' - Whether to disable encryption of the S3 build log output.
newAwsCodeBuildProjectLogsConfigS3LogsDetails ::
  AwsCodeBuildProjectLogsConfigS3LogsDetails
newAwsCodeBuildProjectLogsConfigS3LogsDetails =
  AwsCodeBuildProjectLogsConfigS3LogsDetails'
    { status =
        Prelude.Nothing,
      location = Prelude.Nothing,
      encryptionDisabled =
        Prelude.Nothing
    }

-- | The current status of the S3 build logs.
awsCodeBuildProjectLogsConfigS3LogsDetails_status :: Lens.Lens' AwsCodeBuildProjectLogsConfigS3LogsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectLogsConfigS3LogsDetails_status = Lens.lens (\AwsCodeBuildProjectLogsConfigS3LogsDetails' {status} -> status) (\s@AwsCodeBuildProjectLogsConfigS3LogsDetails' {} a -> s {status = a} :: AwsCodeBuildProjectLogsConfigS3LogsDetails)

-- | The ARN of the S3 bucket and the path prefix for S3 logs.
awsCodeBuildProjectLogsConfigS3LogsDetails_location :: Lens.Lens' AwsCodeBuildProjectLogsConfigS3LogsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectLogsConfigS3LogsDetails_location = Lens.lens (\AwsCodeBuildProjectLogsConfigS3LogsDetails' {location} -> location) (\s@AwsCodeBuildProjectLogsConfigS3LogsDetails' {} a -> s {location = a} :: AwsCodeBuildProjectLogsConfigS3LogsDetails)

-- | Whether to disable encryption of the S3 build log output.
awsCodeBuildProjectLogsConfigS3LogsDetails_encryptionDisabled :: Lens.Lens' AwsCodeBuildProjectLogsConfigS3LogsDetails (Prelude.Maybe Prelude.Bool)
awsCodeBuildProjectLogsConfigS3LogsDetails_encryptionDisabled = Lens.lens (\AwsCodeBuildProjectLogsConfigS3LogsDetails' {encryptionDisabled} -> encryptionDisabled) (\s@AwsCodeBuildProjectLogsConfigS3LogsDetails' {} a -> s {encryptionDisabled = a} :: AwsCodeBuildProjectLogsConfigS3LogsDetails)

instance
  Core.FromJSON
    AwsCodeBuildProjectLogsConfigS3LogsDetails
  where
  parseJSON =
    Core.withObject
      "AwsCodeBuildProjectLogsConfigS3LogsDetails"
      ( \x ->
          AwsCodeBuildProjectLogsConfigS3LogsDetails'
            Prelude.<$> (x Core..:? "Status")
              Prelude.<*> (x Core..:? "Location")
              Prelude.<*> (x Core..:? "EncryptionDisabled")
      )

instance
  Prelude.Hashable
    AwsCodeBuildProjectLogsConfigS3LogsDetails
  where
  hashWithSalt
    salt'
    AwsCodeBuildProjectLogsConfigS3LogsDetails' {..} =
      salt' `Prelude.hashWithSalt` encryptionDisabled
        `Prelude.hashWithSalt` location
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsCodeBuildProjectLogsConfigS3LogsDetails
  where
  rnf AwsCodeBuildProjectLogsConfigS3LogsDetails' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf encryptionDisabled
      `Prelude.seq` Prelude.rnf location

instance
  Core.ToJSON
    AwsCodeBuildProjectLogsConfigS3LogsDetails
  where
  toJSON
    AwsCodeBuildProjectLogsConfigS3LogsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Status" Core..=) Prelude.<$> status,
              ("Location" Core..=) Prelude.<$> location,
              ("EncryptionDisabled" Core..=)
                Prelude.<$> encryptionDisabled
            ]
        )
