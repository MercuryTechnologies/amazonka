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
-- Module      : Amazonka.FinSpace.CreateEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new FinSpace environment.
module Amazonka.FinSpace.CreateEnvironment
  ( -- * Creating a Request
    CreateEnvironment (..),
    newCreateEnvironment,

    -- * Request Lenses
    createEnvironment_federationParameters,
    createEnvironment_federationMode,
    createEnvironment_kmsKeyId,
    createEnvironment_description,
    createEnvironment_tags,
    createEnvironment_name,

    -- * Destructuring the Response
    CreateEnvironmentResponse (..),
    newCreateEnvironmentResponse,

    -- * Response Lenses
    createEnvironmentResponse_environmentUrl,
    createEnvironmentResponse_environmentId,
    createEnvironmentResponse_environmentArn,
    createEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.FinSpace.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { -- | Configuration information when authentication mode is FEDERATED.
    federationParameters :: Prelude.Maybe FederationParameters,
    -- | Authentication mode for the environment.
    --
    -- -   @FEDERATED@ - Users access FinSpace through Single Sign On (SSO) via
    --     your Identity provider.
    --
    -- -   @LOCAL@ - Users access FinSpace via email and password managed
    --     within the FinSpace environment.
    federationMode :: Prelude.Maybe FederationMode,
    -- | The KMS key id to encrypt your data in the FinSpace environment.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The description of the FinSpace environment to be created.
    description :: Prelude.Maybe Prelude.Text,
    -- | Add tags to your FinSpace environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the FinSpace environment to be created.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'federationParameters', 'createEnvironment_federationParameters' - Configuration information when authentication mode is FEDERATED.
--
-- 'federationMode', 'createEnvironment_federationMode' - Authentication mode for the environment.
--
-- -   @FEDERATED@ - Users access FinSpace through Single Sign On (SSO) via
--     your Identity provider.
--
-- -   @LOCAL@ - Users access FinSpace via email and password managed
--     within the FinSpace environment.
--
-- 'kmsKeyId', 'createEnvironment_kmsKeyId' - The KMS key id to encrypt your data in the FinSpace environment.
--
-- 'description', 'createEnvironment_description' - The description of the FinSpace environment to be created.
--
-- 'tags', 'createEnvironment_tags' - Add tags to your FinSpace environment.
--
-- 'name', 'createEnvironment_name' - The name of the FinSpace environment to be created.
newCreateEnvironment ::
  -- | 'name'
  Prelude.Text ->
  CreateEnvironment
newCreateEnvironment pName_ =
  CreateEnvironment'
    { federationParameters =
        Prelude.Nothing,
      federationMode = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | Configuration information when authentication mode is FEDERATED.
createEnvironment_federationParameters :: Lens.Lens' CreateEnvironment (Prelude.Maybe FederationParameters)
createEnvironment_federationParameters = Lens.lens (\CreateEnvironment' {federationParameters} -> federationParameters) (\s@CreateEnvironment' {} a -> s {federationParameters = a} :: CreateEnvironment)

-- | Authentication mode for the environment.
--
-- -   @FEDERATED@ - Users access FinSpace through Single Sign On (SSO) via
--     your Identity provider.
--
-- -   @LOCAL@ - Users access FinSpace via email and password managed
--     within the FinSpace environment.
createEnvironment_federationMode :: Lens.Lens' CreateEnvironment (Prelude.Maybe FederationMode)
createEnvironment_federationMode = Lens.lens (\CreateEnvironment' {federationMode} -> federationMode) (\s@CreateEnvironment' {} a -> s {federationMode = a} :: CreateEnvironment)

-- | The KMS key id to encrypt your data in the FinSpace environment.
createEnvironment_kmsKeyId :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_kmsKeyId = Lens.lens (\CreateEnvironment' {kmsKeyId} -> kmsKeyId) (\s@CreateEnvironment' {} a -> s {kmsKeyId = a} :: CreateEnvironment)

-- | The description of the FinSpace environment to be created.
createEnvironment_description :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_description = Lens.lens (\CreateEnvironment' {description} -> description) (\s@CreateEnvironment' {} a -> s {description = a} :: CreateEnvironment)

-- | Add tags to your FinSpace environment.
createEnvironment_tags :: Lens.Lens' CreateEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEnvironment_tags = Lens.lens (\CreateEnvironment' {tags} -> tags) (\s@CreateEnvironment' {} a -> s {tags = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The name of the FinSpace environment to be created.
createEnvironment_name :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_name = Lens.lens (\CreateEnvironment' {name} -> name) (\s@CreateEnvironment' {} a -> s {name = a} :: CreateEnvironment)

instance Core.AWSRequest CreateEnvironment where
  type
    AWSResponse CreateEnvironment =
      CreateEnvironmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentResponse'
            Prelude.<$> (x Core..?> "environmentUrl")
            Prelude.<*> (x Core..?> "environmentId")
            Prelude.<*> (x Core..?> "environmentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEnvironment where
  hashWithSalt salt' CreateEnvironment' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` federationMode
      `Prelude.hashWithSalt` federationParameters

instance Prelude.NFData CreateEnvironment where
  rnf CreateEnvironment' {..} =
    Prelude.rnf federationParameters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf federationMode

instance Core.ToHeaders CreateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEnvironment where
  toJSON CreateEnvironment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("federationParameters" Core..=)
              Prelude.<$> federationParameters,
            ("federationMode" Core..=)
              Prelude.<$> federationMode,
            ("kmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateEnvironment where
  toPath = Prelude.const "/environment"

instance Core.ToQuery CreateEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEnvironmentResponse' smart constructor.
data CreateEnvironmentResponse = CreateEnvironmentResponse'
  { -- | The sign-in url for the web application of the FinSpace environment you
    -- created.
    environmentUrl :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for FinSpace environment that you created.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the FinSpace environment that you
    -- created.
    environmentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentUrl', 'createEnvironmentResponse_environmentUrl' - The sign-in url for the web application of the FinSpace environment you
-- created.
--
-- 'environmentId', 'createEnvironmentResponse_environmentId' - The unique identifier for FinSpace environment that you created.
--
-- 'environmentArn', 'createEnvironmentResponse_environmentArn' - The Amazon Resource Name (ARN) of the FinSpace environment that you
-- created.
--
-- 'httpStatus', 'createEnvironmentResponse_httpStatus' - The response's http status code.
newCreateEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEnvironmentResponse
newCreateEnvironmentResponse pHttpStatus_ =
  CreateEnvironmentResponse'
    { environmentUrl =
        Prelude.Nothing,
      environmentId = Prelude.Nothing,
      environmentArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The sign-in url for the web application of the FinSpace environment you
-- created.
createEnvironmentResponse_environmentUrl :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.Text)
createEnvironmentResponse_environmentUrl = Lens.lens (\CreateEnvironmentResponse' {environmentUrl} -> environmentUrl) (\s@CreateEnvironmentResponse' {} a -> s {environmentUrl = a} :: CreateEnvironmentResponse)

-- | The unique identifier for FinSpace environment that you created.
createEnvironmentResponse_environmentId :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.Text)
createEnvironmentResponse_environmentId = Lens.lens (\CreateEnvironmentResponse' {environmentId} -> environmentId) (\s@CreateEnvironmentResponse' {} a -> s {environmentId = a} :: CreateEnvironmentResponse)

-- | The Amazon Resource Name (ARN) of the FinSpace environment that you
-- created.
createEnvironmentResponse_environmentArn :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.Text)
createEnvironmentResponse_environmentArn = Lens.lens (\CreateEnvironmentResponse' {environmentArn} -> environmentArn) (\s@CreateEnvironmentResponse' {} a -> s {environmentArn = a} :: CreateEnvironmentResponse)

-- | The response's http status code.
createEnvironmentResponse_httpStatus :: Lens.Lens' CreateEnvironmentResponse Prelude.Int
createEnvironmentResponse_httpStatus = Lens.lens (\CreateEnvironmentResponse' {httpStatus} -> httpStatus) (\s@CreateEnvironmentResponse' {} a -> s {httpStatus = a} :: CreateEnvironmentResponse)

instance Prelude.NFData CreateEnvironmentResponse where
  rnf CreateEnvironmentResponse' {..} =
    Prelude.rnf environmentUrl
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentArn
      `Prelude.seq` Prelude.rnf environmentId
