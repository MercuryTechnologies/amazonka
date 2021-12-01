{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Rekognition.CreateProjectVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a model and begins training. Models are managed
-- as part of an Amazon Rekognition Custom Labels project. You can specify
-- one training dataset and one testing dataset. The response from
-- @CreateProjectVersion@ is an Amazon Resource Name (ARN) for the version
-- of the model.
--
-- Training takes a while to complete. You can get the current status by
-- calling DescribeProjectVersions.
--
-- Once training has successfully completed, call DescribeProjectVersions
-- to get the training results and evaluate the model.
--
-- After evaluating the model, you start the model by calling
-- StartProjectVersion.
--
-- This operation requires permissions to perform the
-- @rekognition:CreateProjectVersion@ action.
module Amazonka.Rekognition.CreateProjectVersion
  ( -- * Creating a Request
    CreateProjectVersion (..),
    newCreateProjectVersion,

    -- * Request Lenses
    createProjectVersion_kmsKeyId,
    createProjectVersion_tags,
    createProjectVersion_projectArn,
    createProjectVersion_versionName,
    createProjectVersion_outputConfig,
    createProjectVersion_trainingData,
    createProjectVersion_testingData,

    -- * Destructuring the Response
    CreateProjectVersionResponse (..),
    newCreateProjectVersionResponse,

    -- * Response Lenses
    createProjectVersionResponse_projectVersionArn,
    createProjectVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProjectVersion' smart constructor.
data CreateProjectVersion = CreateProjectVersion'
  { -- | The identifier for your AWS Key Management Service (AWS KMS) customer
    -- master key (CMK). You can supply the Amazon Resource Name (ARN) of your
    -- CMK, the ID of your CMK, an alias for your CMK, or an alias ARN. The key
    -- is used to encrypt training and test images copied into the service for
    -- model training. Your source images are unaffected. The key is also used
    -- to encrypt training results and manifest files written to the output
    -- Amazon S3 bucket (@OutputConfig@).
    --
    -- If you choose to use your own CMK, you need the following permissions on
    -- the CMK.
    --
    -- -   kms:CreateGrant
    --
    -- -   kms:DescribeKey
    --
    -- -   kms:GenerateDataKey
    --
    -- -   kms:Decrypt
    --
    -- If you don\'t specify a value for @KmsKeyId@, images copied into the
    -- service are encrypted using a key that AWS owns and manages.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A set of tags (key-value pairs) that you want to attach to the model.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the Amazon Rekognition Custom Labels project that manages the
    -- model that you want to train.
    projectArn :: Prelude.Text,
    -- | A name for the version of the model. This value must be unique.
    versionName :: Prelude.Text,
    -- | The Amazon S3 bucket location to store the results of training. The S3
    -- bucket can be in any AWS account as long as the caller has
    -- @s3:PutObject@ permissions on the S3 bucket.
    outputConfig :: OutputConfig,
    -- | The dataset to use for training.
    trainingData :: TrainingData,
    -- | The dataset to use for testing.
    testingData :: TestingData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProjectVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'createProjectVersion_kmsKeyId' - The identifier for your AWS Key Management Service (AWS KMS) customer
-- master key (CMK). You can supply the Amazon Resource Name (ARN) of your
-- CMK, the ID of your CMK, an alias for your CMK, or an alias ARN. The key
-- is used to encrypt training and test images copied into the service for
-- model training. Your source images are unaffected. The key is also used
-- to encrypt training results and manifest files written to the output
-- Amazon S3 bucket (@OutputConfig@).
--
-- If you choose to use your own CMK, you need the following permissions on
-- the CMK.
--
-- -   kms:CreateGrant
--
-- -   kms:DescribeKey
--
-- -   kms:GenerateDataKey
--
-- -   kms:Decrypt
--
-- If you don\'t specify a value for @KmsKeyId@, images copied into the
-- service are encrypted using a key that AWS owns and manages.
--
-- 'tags', 'createProjectVersion_tags' - A set of tags (key-value pairs) that you want to attach to the model.
--
-- 'projectArn', 'createProjectVersion_projectArn' - The ARN of the Amazon Rekognition Custom Labels project that manages the
-- model that you want to train.
--
-- 'versionName', 'createProjectVersion_versionName' - A name for the version of the model. This value must be unique.
--
-- 'outputConfig', 'createProjectVersion_outputConfig' - The Amazon S3 bucket location to store the results of training. The S3
-- bucket can be in any AWS account as long as the caller has
-- @s3:PutObject@ permissions on the S3 bucket.
--
-- 'trainingData', 'createProjectVersion_trainingData' - The dataset to use for training.
--
-- 'testingData', 'createProjectVersion_testingData' - The dataset to use for testing.
newCreateProjectVersion ::
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'versionName'
  Prelude.Text ->
  -- | 'outputConfig'
  OutputConfig ->
  -- | 'trainingData'
  TrainingData ->
  -- | 'testingData'
  TestingData ->
  CreateProjectVersion
newCreateProjectVersion
  pProjectArn_
  pVersionName_
  pOutputConfig_
  pTrainingData_
  pTestingData_ =
    CreateProjectVersion'
      { kmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        projectArn = pProjectArn_,
        versionName = pVersionName_,
        outputConfig = pOutputConfig_,
        trainingData = pTrainingData_,
        testingData = pTestingData_
      }

-- | The identifier for your AWS Key Management Service (AWS KMS) customer
-- master key (CMK). You can supply the Amazon Resource Name (ARN) of your
-- CMK, the ID of your CMK, an alias for your CMK, or an alias ARN. The key
-- is used to encrypt training and test images copied into the service for
-- model training. Your source images are unaffected. The key is also used
-- to encrypt training results and manifest files written to the output
-- Amazon S3 bucket (@OutputConfig@).
--
-- If you choose to use your own CMK, you need the following permissions on
-- the CMK.
--
-- -   kms:CreateGrant
--
-- -   kms:DescribeKey
--
-- -   kms:GenerateDataKey
--
-- -   kms:Decrypt
--
-- If you don\'t specify a value for @KmsKeyId@, images copied into the
-- service are encrypted using a key that AWS owns and manages.
createProjectVersion_kmsKeyId :: Lens.Lens' CreateProjectVersion (Prelude.Maybe Prelude.Text)
createProjectVersion_kmsKeyId = Lens.lens (\CreateProjectVersion' {kmsKeyId} -> kmsKeyId) (\s@CreateProjectVersion' {} a -> s {kmsKeyId = a} :: CreateProjectVersion)

-- | A set of tags (key-value pairs) that you want to attach to the model.
createProjectVersion_tags :: Lens.Lens' CreateProjectVersion (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProjectVersion_tags = Lens.lens (\CreateProjectVersion' {tags} -> tags) (\s@CreateProjectVersion' {} a -> s {tags = a} :: CreateProjectVersion) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the Amazon Rekognition Custom Labels project that manages the
-- model that you want to train.
createProjectVersion_projectArn :: Lens.Lens' CreateProjectVersion Prelude.Text
createProjectVersion_projectArn = Lens.lens (\CreateProjectVersion' {projectArn} -> projectArn) (\s@CreateProjectVersion' {} a -> s {projectArn = a} :: CreateProjectVersion)

-- | A name for the version of the model. This value must be unique.
createProjectVersion_versionName :: Lens.Lens' CreateProjectVersion Prelude.Text
createProjectVersion_versionName = Lens.lens (\CreateProjectVersion' {versionName} -> versionName) (\s@CreateProjectVersion' {} a -> s {versionName = a} :: CreateProjectVersion)

-- | The Amazon S3 bucket location to store the results of training. The S3
-- bucket can be in any AWS account as long as the caller has
-- @s3:PutObject@ permissions on the S3 bucket.
createProjectVersion_outputConfig :: Lens.Lens' CreateProjectVersion OutputConfig
createProjectVersion_outputConfig = Lens.lens (\CreateProjectVersion' {outputConfig} -> outputConfig) (\s@CreateProjectVersion' {} a -> s {outputConfig = a} :: CreateProjectVersion)

-- | The dataset to use for training.
createProjectVersion_trainingData :: Lens.Lens' CreateProjectVersion TrainingData
createProjectVersion_trainingData = Lens.lens (\CreateProjectVersion' {trainingData} -> trainingData) (\s@CreateProjectVersion' {} a -> s {trainingData = a} :: CreateProjectVersion)

-- | The dataset to use for testing.
createProjectVersion_testingData :: Lens.Lens' CreateProjectVersion TestingData
createProjectVersion_testingData = Lens.lens (\CreateProjectVersion' {testingData} -> testingData) (\s@CreateProjectVersion' {} a -> s {testingData = a} :: CreateProjectVersion)

instance Core.AWSRequest CreateProjectVersion where
  type
    AWSResponse CreateProjectVersion =
      CreateProjectVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectVersionResponse'
            Prelude.<$> (x Core..?> "ProjectVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProjectVersion where
  hashWithSalt salt' CreateProjectVersion' {..} =
    salt' `Prelude.hashWithSalt` testingData
      `Prelude.hashWithSalt` trainingData
      `Prelude.hashWithSalt` outputConfig
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData CreateProjectVersion where
  rnf CreateProjectVersion' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf testingData
      `Prelude.seq` Prelude.rnf trainingData
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf tags

instance Core.ToHeaders CreateProjectVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.CreateProjectVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateProjectVersion where
  toJSON CreateProjectVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("ProjectArn" Core..= projectArn),
            Prelude.Just ("VersionName" Core..= versionName),
            Prelude.Just ("OutputConfig" Core..= outputConfig),
            Prelude.Just ("TrainingData" Core..= trainingData),
            Prelude.Just ("TestingData" Core..= testingData)
          ]
      )

instance Core.ToPath CreateProjectVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateProjectVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProjectVersionResponse' smart constructor.
data CreateProjectVersionResponse = CreateProjectVersionResponse'
  { -- | The ARN of the model version that was created. Use
    -- @DescribeProjectVersion@ to get the current status of the training
    -- operation.
    projectVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProjectVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectVersionArn', 'createProjectVersionResponse_projectVersionArn' - The ARN of the model version that was created. Use
-- @DescribeProjectVersion@ to get the current status of the training
-- operation.
--
-- 'httpStatus', 'createProjectVersionResponse_httpStatus' - The response's http status code.
newCreateProjectVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProjectVersionResponse
newCreateProjectVersionResponse pHttpStatus_ =
  CreateProjectVersionResponse'
    { projectVersionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the model version that was created. Use
-- @DescribeProjectVersion@ to get the current status of the training
-- operation.
createProjectVersionResponse_projectVersionArn :: Lens.Lens' CreateProjectVersionResponse (Prelude.Maybe Prelude.Text)
createProjectVersionResponse_projectVersionArn = Lens.lens (\CreateProjectVersionResponse' {projectVersionArn} -> projectVersionArn) (\s@CreateProjectVersionResponse' {} a -> s {projectVersionArn = a} :: CreateProjectVersionResponse)

-- | The response's http status code.
createProjectVersionResponse_httpStatus :: Lens.Lens' CreateProjectVersionResponse Prelude.Int
createProjectVersionResponse_httpStatus = Lens.lens (\CreateProjectVersionResponse' {httpStatus} -> httpStatus) (\s@CreateProjectVersionResponse' {} a -> s {httpStatus = a} :: CreateProjectVersionResponse)

instance Prelude.NFData CreateProjectVersionResponse where
  rnf CreateProjectVersionResponse' {..} =
    Prelude.rnf projectVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
