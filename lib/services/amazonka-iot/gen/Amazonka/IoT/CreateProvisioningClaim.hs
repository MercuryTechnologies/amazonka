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
-- Module      : Amazonka.IoT.CreateProvisioningClaim
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a provisioning claim.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateProvisioningClaim>
-- action.
module Amazonka.IoT.CreateProvisioningClaim
  ( -- * Creating a Request
    CreateProvisioningClaim (..),
    newCreateProvisioningClaim,

    -- * Request Lenses
    createProvisioningClaim_templateName,

    -- * Destructuring the Response
    CreateProvisioningClaimResponse (..),
    newCreateProvisioningClaimResponse,

    -- * Response Lenses
    createProvisioningClaimResponse_keyPair,
    createProvisioningClaimResponse_certificatePem,
    createProvisioningClaimResponse_certificateId,
    createProvisioningClaimResponse_expiration,
    createProvisioningClaimResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProvisioningClaim' smart constructor.
data CreateProvisioningClaim = CreateProvisioningClaim'
  { -- | The name of the provisioning template to use.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisioningClaim' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'createProvisioningClaim_templateName' - The name of the provisioning template to use.
newCreateProvisioningClaim ::
  -- | 'templateName'
  Prelude.Text ->
  CreateProvisioningClaim
newCreateProvisioningClaim pTemplateName_ =
  CreateProvisioningClaim'
    { templateName =
        pTemplateName_
    }

-- | The name of the provisioning template to use.
createProvisioningClaim_templateName :: Lens.Lens' CreateProvisioningClaim Prelude.Text
createProvisioningClaim_templateName = Lens.lens (\CreateProvisioningClaim' {templateName} -> templateName) (\s@CreateProvisioningClaim' {} a -> s {templateName = a} :: CreateProvisioningClaim)

instance Core.AWSRequest CreateProvisioningClaim where
  type
    AWSResponse CreateProvisioningClaim =
      CreateProvisioningClaimResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisioningClaimResponse'
            Prelude.<$> (x Core..?> "keyPair")
            Prelude.<*> (x Core..?> "certificatePem")
            Prelude.<*> (x Core..?> "certificateId")
            Prelude.<*> (x Core..?> "expiration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProvisioningClaim where
  hashWithSalt salt' CreateProvisioningClaim' {..} =
    salt' `Prelude.hashWithSalt` templateName

instance Prelude.NFData CreateProvisioningClaim where
  rnf CreateProvisioningClaim' {..} =
    Prelude.rnf templateName

instance Core.ToHeaders CreateProvisioningClaim where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateProvisioningClaim where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath CreateProvisioningClaim where
  toPath CreateProvisioningClaim' {..} =
    Prelude.mconcat
      [ "/provisioning-templates/",
        Core.toBS templateName,
        "/provisioning-claim"
      ]

instance Core.ToQuery CreateProvisioningClaim where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProvisioningClaimResponse' smart constructor.
data CreateProvisioningClaimResponse = CreateProvisioningClaimResponse'
  { -- | The provisioning claim key pair.
    keyPair :: Prelude.Maybe KeyPair,
    -- | The provisioning claim certificate.
    certificatePem :: Prelude.Maybe Prelude.Text,
    -- | The ID of the certificate.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The provisioning claim expiration time.
    expiration :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisioningClaimResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPair', 'createProvisioningClaimResponse_keyPair' - The provisioning claim key pair.
--
-- 'certificatePem', 'createProvisioningClaimResponse_certificatePem' - The provisioning claim certificate.
--
-- 'certificateId', 'createProvisioningClaimResponse_certificateId' - The ID of the certificate.
--
-- 'expiration', 'createProvisioningClaimResponse_expiration' - The provisioning claim expiration time.
--
-- 'httpStatus', 'createProvisioningClaimResponse_httpStatus' - The response's http status code.
newCreateProvisioningClaimResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProvisioningClaimResponse
newCreateProvisioningClaimResponse pHttpStatus_ =
  CreateProvisioningClaimResponse'
    { keyPair =
        Prelude.Nothing,
      certificatePem = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      expiration = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The provisioning claim key pair.
createProvisioningClaimResponse_keyPair :: Lens.Lens' CreateProvisioningClaimResponse (Prelude.Maybe KeyPair)
createProvisioningClaimResponse_keyPair = Lens.lens (\CreateProvisioningClaimResponse' {keyPair} -> keyPair) (\s@CreateProvisioningClaimResponse' {} a -> s {keyPair = a} :: CreateProvisioningClaimResponse)

-- | The provisioning claim certificate.
createProvisioningClaimResponse_certificatePem :: Lens.Lens' CreateProvisioningClaimResponse (Prelude.Maybe Prelude.Text)
createProvisioningClaimResponse_certificatePem = Lens.lens (\CreateProvisioningClaimResponse' {certificatePem} -> certificatePem) (\s@CreateProvisioningClaimResponse' {} a -> s {certificatePem = a} :: CreateProvisioningClaimResponse)

-- | The ID of the certificate.
createProvisioningClaimResponse_certificateId :: Lens.Lens' CreateProvisioningClaimResponse (Prelude.Maybe Prelude.Text)
createProvisioningClaimResponse_certificateId = Lens.lens (\CreateProvisioningClaimResponse' {certificateId} -> certificateId) (\s@CreateProvisioningClaimResponse' {} a -> s {certificateId = a} :: CreateProvisioningClaimResponse)

-- | The provisioning claim expiration time.
createProvisioningClaimResponse_expiration :: Lens.Lens' CreateProvisioningClaimResponse (Prelude.Maybe Prelude.UTCTime)
createProvisioningClaimResponse_expiration = Lens.lens (\CreateProvisioningClaimResponse' {expiration} -> expiration) (\s@CreateProvisioningClaimResponse' {} a -> s {expiration = a} :: CreateProvisioningClaimResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
createProvisioningClaimResponse_httpStatus :: Lens.Lens' CreateProvisioningClaimResponse Prelude.Int
createProvisioningClaimResponse_httpStatus = Lens.lens (\CreateProvisioningClaimResponse' {httpStatus} -> httpStatus) (\s@CreateProvisioningClaimResponse' {} a -> s {httpStatus = a} :: CreateProvisioningClaimResponse)

instance
  Prelude.NFData
    CreateProvisioningClaimResponse
  where
  rnf CreateProvisioningClaimResponse' {..} =
    Prelude.rnf keyPair
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf certificatePem
