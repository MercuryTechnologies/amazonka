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
-- Module      : Amazonka.SageMaker.Types.AsyncInferenceOutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AsyncInferenceOutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AsyncInferenceNotificationConfig

-- | Specifies the configuration for asynchronous inference invocation
-- outputs.
--
-- /See:/ 'newAsyncInferenceOutputConfig' smart constructor.
data AsyncInferenceOutputConfig = AsyncInferenceOutputConfig'
  { -- | Specifies the configuration for notifications of inference results for
    -- asynchronous inference.
    notificationConfig :: Prelude.Maybe AsyncInferenceNotificationConfig,
    -- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
    -- key that Amazon SageMaker uses to encrypt the asynchronous inference
    -- output in Amazon S3.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location to upload inference responses to.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AsyncInferenceOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationConfig', 'asyncInferenceOutputConfig_notificationConfig' - Specifies the configuration for notifications of inference results for
-- asynchronous inference.
--
-- 'kmsKeyId', 'asyncInferenceOutputConfig_kmsKeyId' - The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt the asynchronous inference
-- output in Amazon S3.
--
-- 's3OutputPath', 'asyncInferenceOutputConfig_s3OutputPath' - The Amazon S3 location to upload inference responses to.
newAsyncInferenceOutputConfig ::
  -- | 's3OutputPath'
  Prelude.Text ->
  AsyncInferenceOutputConfig
newAsyncInferenceOutputConfig pS3OutputPath_ =
  AsyncInferenceOutputConfig'
    { notificationConfig =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | Specifies the configuration for notifications of inference results for
-- asynchronous inference.
asyncInferenceOutputConfig_notificationConfig :: Lens.Lens' AsyncInferenceOutputConfig (Prelude.Maybe AsyncInferenceNotificationConfig)
asyncInferenceOutputConfig_notificationConfig = Lens.lens (\AsyncInferenceOutputConfig' {notificationConfig} -> notificationConfig) (\s@AsyncInferenceOutputConfig' {} a -> s {notificationConfig = a} :: AsyncInferenceOutputConfig)

-- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt the asynchronous inference
-- output in Amazon S3.
asyncInferenceOutputConfig_kmsKeyId :: Lens.Lens' AsyncInferenceOutputConfig (Prelude.Maybe Prelude.Text)
asyncInferenceOutputConfig_kmsKeyId = Lens.lens (\AsyncInferenceOutputConfig' {kmsKeyId} -> kmsKeyId) (\s@AsyncInferenceOutputConfig' {} a -> s {kmsKeyId = a} :: AsyncInferenceOutputConfig)

-- | The Amazon S3 location to upload inference responses to.
asyncInferenceOutputConfig_s3OutputPath :: Lens.Lens' AsyncInferenceOutputConfig Prelude.Text
asyncInferenceOutputConfig_s3OutputPath = Lens.lens (\AsyncInferenceOutputConfig' {s3OutputPath} -> s3OutputPath) (\s@AsyncInferenceOutputConfig' {} a -> s {s3OutputPath = a} :: AsyncInferenceOutputConfig)

instance Core.FromJSON AsyncInferenceOutputConfig where
  parseJSON =
    Core.withObject
      "AsyncInferenceOutputConfig"
      ( \x ->
          AsyncInferenceOutputConfig'
            Prelude.<$> (x Core..:? "NotificationConfig")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..: "S3OutputPath")
      )

instance Prelude.Hashable AsyncInferenceOutputConfig where
  hashWithSalt salt' AsyncInferenceOutputConfig' {..} =
    salt' `Prelude.hashWithSalt` s3OutputPath
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` notificationConfig

instance Prelude.NFData AsyncInferenceOutputConfig where
  rnf AsyncInferenceOutputConfig' {..} =
    Prelude.rnf notificationConfig
      `Prelude.seq` Prelude.rnf s3OutputPath
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Core.ToJSON AsyncInferenceOutputConfig where
  toJSON AsyncInferenceOutputConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NotificationConfig" Core..=)
              Prelude.<$> notificationConfig,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            Prelude.Just ("S3OutputPath" Core..= s3OutputPath)
          ]
      )
