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
-- Module      : Amazonka.Transcribe.CreateLanguageModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom language model. Use Amazon S3 prefixes to provide
-- the location of your input files. The time it takes to create your model
-- depends on the size of your training data.
module Amazonka.Transcribe.CreateLanguageModel
  ( -- * Creating a Request
    CreateLanguageModel (..),
    newCreateLanguageModel,

    -- * Request Lenses
    createLanguageModel_tags,
    createLanguageModel_languageCode,
    createLanguageModel_baseModelName,
    createLanguageModel_modelName,
    createLanguageModel_inputDataConfig,

    -- * Destructuring the Response
    CreateLanguageModelResponse (..),
    newCreateLanguageModelResponse,

    -- * Response Lenses
    createLanguageModelResponse_languageCode,
    createLanguageModelResponse_modelName,
    createLanguageModelResponse_inputDataConfig,
    createLanguageModelResponse_baseModelName,
    createLanguageModelResponse_modelStatus,
    createLanguageModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newCreateLanguageModel' smart constructor.
data CreateLanguageModel = CreateLanguageModel'
  { -- | Adds one or more tags, each in the form of a key:value pair, to a new
    -- language model at the time you create this new model.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The language of the input text you\'re using to train your custom
    -- language model.
    languageCode :: CLMLanguageCode,
    -- | The Amazon Transcribe standard language model, or base model used to
    -- create your custom language model.
    --
    -- If you want to use your custom language model to transcribe audio with a
    -- sample rate of 16,000 Hz or greater, choose @Wideband@.
    --
    -- If you want to use your custom language model to transcribe audio with a
    -- sample rate that is less than 16,000 Hz, choose @Narrowband@.
    baseModelName :: BaseModelName,
    -- | The name you choose for your custom language model when you create it.
    modelName :: Prelude.Text,
    -- | Contains the data access role and the Amazon S3 prefixes to read the
    -- required input files to create a custom language model.
    inputDataConfig :: InputDataConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLanguageModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLanguageModel_tags' - Adds one or more tags, each in the form of a key:value pair, to a new
-- language model at the time you create this new model.
--
-- 'languageCode', 'createLanguageModel_languageCode' - The language of the input text you\'re using to train your custom
-- language model.
--
-- 'baseModelName', 'createLanguageModel_baseModelName' - The Amazon Transcribe standard language model, or base model used to
-- create your custom language model.
--
-- If you want to use your custom language model to transcribe audio with a
-- sample rate of 16,000 Hz or greater, choose @Wideband@.
--
-- If you want to use your custom language model to transcribe audio with a
-- sample rate that is less than 16,000 Hz, choose @Narrowband@.
--
-- 'modelName', 'createLanguageModel_modelName' - The name you choose for your custom language model when you create it.
--
-- 'inputDataConfig', 'createLanguageModel_inputDataConfig' - Contains the data access role and the Amazon S3 prefixes to read the
-- required input files to create a custom language model.
newCreateLanguageModel ::
  -- | 'languageCode'
  CLMLanguageCode ->
  -- | 'baseModelName'
  BaseModelName ->
  -- | 'modelName'
  Prelude.Text ->
  -- | 'inputDataConfig'
  InputDataConfig ->
  CreateLanguageModel
newCreateLanguageModel
  pLanguageCode_
  pBaseModelName_
  pModelName_
  pInputDataConfig_ =
    CreateLanguageModel'
      { tags = Prelude.Nothing,
        languageCode = pLanguageCode_,
        baseModelName = pBaseModelName_,
        modelName = pModelName_,
        inputDataConfig = pInputDataConfig_
      }

-- | Adds one or more tags, each in the form of a key:value pair, to a new
-- language model at the time you create this new model.
createLanguageModel_tags :: Lens.Lens' CreateLanguageModel (Prelude.Maybe (Prelude.NonEmpty Tag))
createLanguageModel_tags = Lens.lens (\CreateLanguageModel' {tags} -> tags) (\s@CreateLanguageModel' {} a -> s {tags = a} :: CreateLanguageModel) Prelude.. Lens.mapping Lens.coerced

-- | The language of the input text you\'re using to train your custom
-- language model.
createLanguageModel_languageCode :: Lens.Lens' CreateLanguageModel CLMLanguageCode
createLanguageModel_languageCode = Lens.lens (\CreateLanguageModel' {languageCode} -> languageCode) (\s@CreateLanguageModel' {} a -> s {languageCode = a} :: CreateLanguageModel)

-- | The Amazon Transcribe standard language model, or base model used to
-- create your custom language model.
--
-- If you want to use your custom language model to transcribe audio with a
-- sample rate of 16,000 Hz or greater, choose @Wideband@.
--
-- If you want to use your custom language model to transcribe audio with a
-- sample rate that is less than 16,000 Hz, choose @Narrowband@.
createLanguageModel_baseModelName :: Lens.Lens' CreateLanguageModel BaseModelName
createLanguageModel_baseModelName = Lens.lens (\CreateLanguageModel' {baseModelName} -> baseModelName) (\s@CreateLanguageModel' {} a -> s {baseModelName = a} :: CreateLanguageModel)

-- | The name you choose for your custom language model when you create it.
createLanguageModel_modelName :: Lens.Lens' CreateLanguageModel Prelude.Text
createLanguageModel_modelName = Lens.lens (\CreateLanguageModel' {modelName} -> modelName) (\s@CreateLanguageModel' {} a -> s {modelName = a} :: CreateLanguageModel)

-- | Contains the data access role and the Amazon S3 prefixes to read the
-- required input files to create a custom language model.
createLanguageModel_inputDataConfig :: Lens.Lens' CreateLanguageModel InputDataConfig
createLanguageModel_inputDataConfig = Lens.lens (\CreateLanguageModel' {inputDataConfig} -> inputDataConfig) (\s@CreateLanguageModel' {} a -> s {inputDataConfig = a} :: CreateLanguageModel)

instance Core.AWSRequest CreateLanguageModel where
  type
    AWSResponse CreateLanguageModel =
      CreateLanguageModelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLanguageModelResponse'
            Prelude.<$> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "ModelName")
            Prelude.<*> (x Core..?> "InputDataConfig")
            Prelude.<*> (x Core..?> "BaseModelName")
            Prelude.<*> (x Core..?> "ModelStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLanguageModel where
  hashWithSalt salt' CreateLanguageModel' {..} =
    salt' `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` baseModelName
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateLanguageModel where
  rnf CreateLanguageModel' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf baseModelName
      `Prelude.seq` Prelude.rnf languageCode

instance Core.ToHeaders CreateLanguageModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.CreateLanguageModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLanguageModel where
  toJSON CreateLanguageModel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("LanguageCode" Core..= languageCode),
            Prelude.Just ("BaseModelName" Core..= baseModelName),
            Prelude.Just ("ModelName" Core..= modelName),
            Prelude.Just
              ("InputDataConfig" Core..= inputDataConfig)
          ]
      )

instance Core.ToPath CreateLanguageModel where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateLanguageModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLanguageModelResponse' smart constructor.
data CreateLanguageModelResponse = CreateLanguageModelResponse'
  { -- | The language code of the text you\'ve used to create a custom language
    -- model.
    languageCode :: Prelude.Maybe CLMLanguageCode,
    -- | The name you\'ve chosen for your custom language model.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The data access role and Amazon S3 prefixes you\'ve chosen to create
    -- your custom language model.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The Amazon Transcribe standard language model, or base model you\'ve
    -- used to create a custom language model.
    baseModelName :: Prelude.Maybe BaseModelName,
    -- | The status of the custom language model. When the status is @COMPLETED@
    -- the model is ready to use.
    modelStatus :: Prelude.Maybe ModelStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLanguageModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'createLanguageModelResponse_languageCode' - The language code of the text you\'ve used to create a custom language
-- model.
--
-- 'modelName', 'createLanguageModelResponse_modelName' - The name you\'ve chosen for your custom language model.
--
-- 'inputDataConfig', 'createLanguageModelResponse_inputDataConfig' - The data access role and Amazon S3 prefixes you\'ve chosen to create
-- your custom language model.
--
-- 'baseModelName', 'createLanguageModelResponse_baseModelName' - The Amazon Transcribe standard language model, or base model you\'ve
-- used to create a custom language model.
--
-- 'modelStatus', 'createLanguageModelResponse_modelStatus' - The status of the custom language model. When the status is @COMPLETED@
-- the model is ready to use.
--
-- 'httpStatus', 'createLanguageModelResponse_httpStatus' - The response's http status code.
newCreateLanguageModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLanguageModelResponse
newCreateLanguageModelResponse pHttpStatus_ =
  CreateLanguageModelResponse'
    { languageCode =
        Prelude.Nothing,
      modelName = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      baseModelName = Prelude.Nothing,
      modelStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The language code of the text you\'ve used to create a custom language
-- model.
createLanguageModelResponse_languageCode :: Lens.Lens' CreateLanguageModelResponse (Prelude.Maybe CLMLanguageCode)
createLanguageModelResponse_languageCode = Lens.lens (\CreateLanguageModelResponse' {languageCode} -> languageCode) (\s@CreateLanguageModelResponse' {} a -> s {languageCode = a} :: CreateLanguageModelResponse)

-- | The name you\'ve chosen for your custom language model.
createLanguageModelResponse_modelName :: Lens.Lens' CreateLanguageModelResponse (Prelude.Maybe Prelude.Text)
createLanguageModelResponse_modelName = Lens.lens (\CreateLanguageModelResponse' {modelName} -> modelName) (\s@CreateLanguageModelResponse' {} a -> s {modelName = a} :: CreateLanguageModelResponse)

-- | The data access role and Amazon S3 prefixes you\'ve chosen to create
-- your custom language model.
createLanguageModelResponse_inputDataConfig :: Lens.Lens' CreateLanguageModelResponse (Prelude.Maybe InputDataConfig)
createLanguageModelResponse_inputDataConfig = Lens.lens (\CreateLanguageModelResponse' {inputDataConfig} -> inputDataConfig) (\s@CreateLanguageModelResponse' {} a -> s {inputDataConfig = a} :: CreateLanguageModelResponse)

-- | The Amazon Transcribe standard language model, or base model you\'ve
-- used to create a custom language model.
createLanguageModelResponse_baseModelName :: Lens.Lens' CreateLanguageModelResponse (Prelude.Maybe BaseModelName)
createLanguageModelResponse_baseModelName = Lens.lens (\CreateLanguageModelResponse' {baseModelName} -> baseModelName) (\s@CreateLanguageModelResponse' {} a -> s {baseModelName = a} :: CreateLanguageModelResponse)

-- | The status of the custom language model. When the status is @COMPLETED@
-- the model is ready to use.
createLanguageModelResponse_modelStatus :: Lens.Lens' CreateLanguageModelResponse (Prelude.Maybe ModelStatus)
createLanguageModelResponse_modelStatus = Lens.lens (\CreateLanguageModelResponse' {modelStatus} -> modelStatus) (\s@CreateLanguageModelResponse' {} a -> s {modelStatus = a} :: CreateLanguageModelResponse)

-- | The response's http status code.
createLanguageModelResponse_httpStatus :: Lens.Lens' CreateLanguageModelResponse Prelude.Int
createLanguageModelResponse_httpStatus = Lens.lens (\CreateLanguageModelResponse' {httpStatus} -> httpStatus) (\s@CreateLanguageModelResponse' {} a -> s {httpStatus = a} :: CreateLanguageModelResponse)

instance Prelude.NFData CreateLanguageModelResponse where
  rnf CreateLanguageModelResponse' {..} =
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelStatus
      `Prelude.seq` Prelude.rnf baseModelName
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf modelName
