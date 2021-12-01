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
-- Module      : Amazonka.ImageBuilder.CreateComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new component that can be used to build, validate, test, and
-- assess your image.
module Amazonka.ImageBuilder.CreateComponent
  ( -- * Creating a Request
    CreateComponent (..),
    newCreateComponent,

    -- * Request Lenses
    createComponent_data,
    createComponent_supportedOsVersions,
    createComponent_uri,
    createComponent_kmsKeyId,
    createComponent_changeDescription,
    createComponent_description,
    createComponent_tags,
    createComponent_name,
    createComponent_semanticVersion,
    createComponent_platform,
    createComponent_clientToken,

    -- * Destructuring the Response
    CreateComponentResponse (..),
    newCreateComponentResponse,

    -- * Response Lenses
    createComponentResponse_requestId,
    createComponentResponse_clientToken,
    createComponentResponse_componentBuildVersionArn,
    createComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateComponent' smart constructor.
data CreateComponent = CreateComponent'
  { -- | The data of the component. Used to specify the data inline. Either
    -- @data@ or @uri@ can be used to specify the data within the component.
    data' :: Prelude.Maybe Prelude.Text,
    -- | The operating system (OS) version supported by the component. If the OS
    -- information is available, a prefix match is performed against the base
    -- image OS version during image recipe creation.
    supportedOsVersions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The uri of the component. Must be an Amazon S3 URL and the requester
    -- must have permission to access the Amazon S3 bucket. If you use Amazon
    -- S3, you can specify component content up to your service quota. Either
    -- @data@ or @uri@ can be used to specify the data within the component.
    uri :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS key that should be used to encrypt this component.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The change description of the component. Describes what change has been
    -- made in this version, or what makes this version different from other
    -- versions of this component.
    changeDescription :: Prelude.Maybe Prelude.Text,
    -- | The description of the component. Describes the contents of the
    -- component.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags of the component.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the component.
    name :: Prelude.Text,
    -- | The semantic version of the component. This version follows the semantic
    -- version syntax.
    --
    -- The semantic version has four nodes:
    -- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
    -- first three, and can filter on all of them.
    --
    -- __Assignment:__ For the first three nodes you can assign any positive
    -- integer value, including zero, with an upper limit of 2^30-1, or
    -- 1073741823 for each node. Image Builder automatically assigns the build
    -- number to the fourth node.
    --
    -- __Patterns:__ You can use any numeric pattern that adheres to the
    -- assignment requirements for the nodes that you can assign. For example,
    -- you might choose a software version pattern, such as 1.0.0, or a date,
    -- such as 2021.01.01.
    semanticVersion :: Prelude.Text,
    -- | The platform of the component.
    platform :: Platform,
    -- | The idempotency token of the component.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'createComponent_data' - The data of the component. Used to specify the data inline. Either
-- @data@ or @uri@ can be used to specify the data within the component.
--
-- 'supportedOsVersions', 'createComponent_supportedOsVersions' - The operating system (OS) version supported by the component. If the OS
-- information is available, a prefix match is performed against the base
-- image OS version during image recipe creation.
--
-- 'uri', 'createComponent_uri' - The uri of the component. Must be an Amazon S3 URL and the requester
-- must have permission to access the Amazon S3 bucket. If you use Amazon
-- S3, you can specify component content up to your service quota. Either
-- @data@ or @uri@ can be used to specify the data within the component.
--
-- 'kmsKeyId', 'createComponent_kmsKeyId' - The ID of the KMS key that should be used to encrypt this component.
--
-- 'changeDescription', 'createComponent_changeDescription' - The change description of the component. Describes what change has been
-- made in this version, or what makes this version different from other
-- versions of this component.
--
-- 'description', 'createComponent_description' - The description of the component. Describes the contents of the
-- component.
--
-- 'tags', 'createComponent_tags' - The tags of the component.
--
-- 'name', 'createComponent_name' - The name of the component.
--
-- 'semanticVersion', 'createComponent_semanticVersion' - The semantic version of the component. This version follows the semantic
-- version syntax.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Assignment:__ For the first three nodes you can assign any positive
-- integer value, including zero, with an upper limit of 2^30-1, or
-- 1073741823 for each node. Image Builder automatically assigns the build
-- number to the fourth node.
--
-- __Patterns:__ You can use any numeric pattern that adheres to the
-- assignment requirements for the nodes that you can assign. For example,
-- you might choose a software version pattern, such as 1.0.0, or a date,
-- such as 2021.01.01.
--
-- 'platform', 'createComponent_platform' - The platform of the component.
--
-- 'clientToken', 'createComponent_clientToken' - The idempotency token of the component.
newCreateComponent ::
  -- | 'name'
  Prelude.Text ->
  -- | 'semanticVersion'
  Prelude.Text ->
  -- | 'platform'
  Platform ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateComponent
newCreateComponent
  pName_
  pSemanticVersion_
  pPlatform_
  pClientToken_ =
    CreateComponent'
      { data' = Prelude.Nothing,
        supportedOsVersions = Prelude.Nothing,
        uri = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        changeDescription = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        semanticVersion = pSemanticVersion_,
        platform = pPlatform_,
        clientToken = pClientToken_
      }

-- | The data of the component. Used to specify the data inline. Either
-- @data@ or @uri@ can be used to specify the data within the component.
createComponent_data :: Lens.Lens' CreateComponent (Prelude.Maybe Prelude.Text)
createComponent_data = Lens.lens (\CreateComponent' {data'} -> data') (\s@CreateComponent' {} a -> s {data' = a} :: CreateComponent)

-- | The operating system (OS) version supported by the component. If the OS
-- information is available, a prefix match is performed against the base
-- image OS version during image recipe creation.
createComponent_supportedOsVersions :: Lens.Lens' CreateComponent (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createComponent_supportedOsVersions = Lens.lens (\CreateComponent' {supportedOsVersions} -> supportedOsVersions) (\s@CreateComponent' {} a -> s {supportedOsVersions = a} :: CreateComponent) Prelude.. Lens.mapping Lens.coerced

-- | The uri of the component. Must be an Amazon S3 URL and the requester
-- must have permission to access the Amazon S3 bucket. If you use Amazon
-- S3, you can specify component content up to your service quota. Either
-- @data@ or @uri@ can be used to specify the data within the component.
createComponent_uri :: Lens.Lens' CreateComponent (Prelude.Maybe Prelude.Text)
createComponent_uri = Lens.lens (\CreateComponent' {uri} -> uri) (\s@CreateComponent' {} a -> s {uri = a} :: CreateComponent)

-- | The ID of the KMS key that should be used to encrypt this component.
createComponent_kmsKeyId :: Lens.Lens' CreateComponent (Prelude.Maybe Prelude.Text)
createComponent_kmsKeyId = Lens.lens (\CreateComponent' {kmsKeyId} -> kmsKeyId) (\s@CreateComponent' {} a -> s {kmsKeyId = a} :: CreateComponent)

-- | The change description of the component. Describes what change has been
-- made in this version, or what makes this version different from other
-- versions of this component.
createComponent_changeDescription :: Lens.Lens' CreateComponent (Prelude.Maybe Prelude.Text)
createComponent_changeDescription = Lens.lens (\CreateComponent' {changeDescription} -> changeDescription) (\s@CreateComponent' {} a -> s {changeDescription = a} :: CreateComponent)

-- | The description of the component. Describes the contents of the
-- component.
createComponent_description :: Lens.Lens' CreateComponent (Prelude.Maybe Prelude.Text)
createComponent_description = Lens.lens (\CreateComponent' {description} -> description) (\s@CreateComponent' {} a -> s {description = a} :: CreateComponent)

-- | The tags of the component.
createComponent_tags :: Lens.Lens' CreateComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createComponent_tags = Lens.lens (\CreateComponent' {tags} -> tags) (\s@CreateComponent' {} a -> s {tags = a} :: CreateComponent) Prelude.. Lens.mapping Lens.coerced

-- | The name of the component.
createComponent_name :: Lens.Lens' CreateComponent Prelude.Text
createComponent_name = Lens.lens (\CreateComponent' {name} -> name) (\s@CreateComponent' {} a -> s {name = a} :: CreateComponent)

-- | The semantic version of the component. This version follows the semantic
-- version syntax.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Assignment:__ For the first three nodes you can assign any positive
-- integer value, including zero, with an upper limit of 2^30-1, or
-- 1073741823 for each node. Image Builder automatically assigns the build
-- number to the fourth node.
--
-- __Patterns:__ You can use any numeric pattern that adheres to the
-- assignment requirements for the nodes that you can assign. For example,
-- you might choose a software version pattern, such as 1.0.0, or a date,
-- such as 2021.01.01.
createComponent_semanticVersion :: Lens.Lens' CreateComponent Prelude.Text
createComponent_semanticVersion = Lens.lens (\CreateComponent' {semanticVersion} -> semanticVersion) (\s@CreateComponent' {} a -> s {semanticVersion = a} :: CreateComponent)

-- | The platform of the component.
createComponent_platform :: Lens.Lens' CreateComponent Platform
createComponent_platform = Lens.lens (\CreateComponent' {platform} -> platform) (\s@CreateComponent' {} a -> s {platform = a} :: CreateComponent)

-- | The idempotency token of the component.
createComponent_clientToken :: Lens.Lens' CreateComponent Prelude.Text
createComponent_clientToken = Lens.lens (\CreateComponent' {clientToken} -> clientToken) (\s@CreateComponent' {} a -> s {clientToken = a} :: CreateComponent)

instance Core.AWSRequest CreateComponent where
  type
    AWSResponse CreateComponent =
      CreateComponentResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateComponentResponse'
            Prelude.<$> (x Core..?> "requestId")
            Prelude.<*> (x Core..?> "clientToken")
            Prelude.<*> (x Core..?> "componentBuildVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateComponent where
  hashWithSalt salt' CreateComponent' {..} =
    salt' `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` semanticVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` changeDescription
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` supportedOsVersions
      `Prelude.hashWithSalt` data'

instance Prelude.NFData CreateComponent where
  rnf CreateComponent' {..} =
    Prelude.rnf data'
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf semanticVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf changeDescription
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf uri
      `Prelude.seq` Prelude.rnf supportedOsVersions

instance Core.ToHeaders CreateComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateComponent where
  toJSON CreateComponent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("data" Core..=) Prelude.<$> data',
            ("supportedOsVersions" Core..=)
              Prelude.<$> supportedOsVersions,
            ("uri" Core..=) Prelude.<$> uri,
            ("kmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("changeDescription" Core..=)
              Prelude.<$> changeDescription,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("semanticVersion" Core..= semanticVersion),
            Prelude.Just ("platform" Core..= platform),
            Prelude.Just ("clientToken" Core..= clientToken)
          ]
      )

instance Core.ToPath CreateComponent where
  toPath = Prelude.const "/CreateComponent"

instance Core.ToQuery CreateComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateComponentResponse' smart constructor.
data CreateComponentResponse = CreateComponentResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the component that was created by this
    -- request.
    componentBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'createComponentResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'clientToken', 'createComponentResponse_clientToken' - The idempotency token used to make this request idempotent.
--
-- 'componentBuildVersionArn', 'createComponentResponse_componentBuildVersionArn' - The Amazon Resource Name (ARN) of the component that was created by this
-- request.
--
-- 'httpStatus', 'createComponentResponse_httpStatus' - The response's http status code.
newCreateComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateComponentResponse
newCreateComponentResponse pHttpStatus_ =
  CreateComponentResponse'
    { requestId =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      componentBuildVersionArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request ID that uniquely identifies this request.
createComponentResponse_requestId :: Lens.Lens' CreateComponentResponse (Prelude.Maybe Prelude.Text)
createComponentResponse_requestId = Lens.lens (\CreateComponentResponse' {requestId} -> requestId) (\s@CreateComponentResponse' {} a -> s {requestId = a} :: CreateComponentResponse)

-- | The idempotency token used to make this request idempotent.
createComponentResponse_clientToken :: Lens.Lens' CreateComponentResponse (Prelude.Maybe Prelude.Text)
createComponentResponse_clientToken = Lens.lens (\CreateComponentResponse' {clientToken} -> clientToken) (\s@CreateComponentResponse' {} a -> s {clientToken = a} :: CreateComponentResponse)

-- | The Amazon Resource Name (ARN) of the component that was created by this
-- request.
createComponentResponse_componentBuildVersionArn :: Lens.Lens' CreateComponentResponse (Prelude.Maybe Prelude.Text)
createComponentResponse_componentBuildVersionArn = Lens.lens (\CreateComponentResponse' {componentBuildVersionArn} -> componentBuildVersionArn) (\s@CreateComponentResponse' {} a -> s {componentBuildVersionArn = a} :: CreateComponentResponse)

-- | The response's http status code.
createComponentResponse_httpStatus :: Lens.Lens' CreateComponentResponse Prelude.Int
createComponentResponse_httpStatus = Lens.lens (\CreateComponentResponse' {httpStatus} -> httpStatus) (\s@CreateComponentResponse' {} a -> s {httpStatus = a} :: CreateComponentResponse)

instance Prelude.NFData CreateComponentResponse where
  rnf CreateComponentResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf componentBuildVersionArn
      `Prelude.seq` Prelude.rnf clientToken
