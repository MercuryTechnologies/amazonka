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
-- Module      : Amazonka.IoT.RegisterCACertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a CA certificate with IoT. This CA certificate can then be
-- used to sign device certificates, which can be then registered with IoT.
-- You can register up to 10 CA certificates per Amazon Web Services
-- account that have the same subject field. This enables you to have up to
-- 10 certificate authorities sign your device certificates. If you have
-- more than one CA certificate registered, make sure you pass the CA
-- certificate when you register your device certificates with the
-- RegisterCertificate action.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions RegisterCACertificate>
-- action.
module Amazonka.IoT.RegisterCACertificate
  ( -- * Creating a Request
    RegisterCACertificate (..),
    newRegisterCACertificate,

    -- * Request Lenses
    registerCACertificate_setAsActive,
    registerCACertificate_allowAutoRegistration,
    registerCACertificate_registrationConfig,
    registerCACertificate_tags,
    registerCACertificate_caCertificate,
    registerCACertificate_verificationCertificate,

    -- * Destructuring the Response
    RegisterCACertificateResponse (..),
    newRegisterCACertificateResponse,

    -- * Response Lenses
    registerCACertificateResponse_certificateArn,
    registerCACertificateResponse_certificateId,
    registerCACertificateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input to the RegisterCACertificate operation.
--
-- /See:/ 'newRegisterCACertificate' smart constructor.
data RegisterCACertificate = RegisterCACertificate'
  { -- | A boolean value that specifies if the CA certificate is set to active.
    setAsActive :: Prelude.Maybe Prelude.Bool,
    -- | Allows this CA certificate to be used for auto registration of device
    -- certificates.
    allowAutoRegistration :: Prelude.Maybe Prelude.Bool,
    -- | Information about the registration configuration.
    registrationConfig :: Prelude.Maybe RegistrationConfig,
    -- | Metadata which can be used to manage the CA certificate.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: &&tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Prelude.Maybe [Tag],
    -- | The CA certificate.
    caCertificate :: Prelude.Text,
    -- | The private key verification certificate.
    verificationCertificate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterCACertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setAsActive', 'registerCACertificate_setAsActive' - A boolean value that specifies if the CA certificate is set to active.
--
-- 'allowAutoRegistration', 'registerCACertificate_allowAutoRegistration' - Allows this CA certificate to be used for auto registration of device
-- certificates.
--
-- 'registrationConfig', 'registerCACertificate_registrationConfig' - Information about the registration configuration.
--
-- 'tags', 'registerCACertificate_tags' - Metadata which can be used to manage the CA certificate.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
--
-- 'caCertificate', 'registerCACertificate_caCertificate' - The CA certificate.
--
-- 'verificationCertificate', 'registerCACertificate_verificationCertificate' - The private key verification certificate.
newRegisterCACertificate ::
  -- | 'caCertificate'
  Prelude.Text ->
  -- | 'verificationCertificate'
  Prelude.Text ->
  RegisterCACertificate
newRegisterCACertificate
  pCaCertificate_
  pVerificationCertificate_ =
    RegisterCACertificate'
      { setAsActive =
          Prelude.Nothing,
        allowAutoRegistration = Prelude.Nothing,
        registrationConfig = Prelude.Nothing,
        tags = Prelude.Nothing,
        caCertificate = pCaCertificate_,
        verificationCertificate = pVerificationCertificate_
      }

-- | A boolean value that specifies if the CA certificate is set to active.
registerCACertificate_setAsActive :: Lens.Lens' RegisterCACertificate (Prelude.Maybe Prelude.Bool)
registerCACertificate_setAsActive = Lens.lens (\RegisterCACertificate' {setAsActive} -> setAsActive) (\s@RegisterCACertificate' {} a -> s {setAsActive = a} :: RegisterCACertificate)

-- | Allows this CA certificate to be used for auto registration of device
-- certificates.
registerCACertificate_allowAutoRegistration :: Lens.Lens' RegisterCACertificate (Prelude.Maybe Prelude.Bool)
registerCACertificate_allowAutoRegistration = Lens.lens (\RegisterCACertificate' {allowAutoRegistration} -> allowAutoRegistration) (\s@RegisterCACertificate' {} a -> s {allowAutoRegistration = a} :: RegisterCACertificate)

-- | Information about the registration configuration.
registerCACertificate_registrationConfig :: Lens.Lens' RegisterCACertificate (Prelude.Maybe RegistrationConfig)
registerCACertificate_registrationConfig = Lens.lens (\RegisterCACertificate' {registrationConfig} -> registrationConfig) (\s@RegisterCACertificate' {} a -> s {registrationConfig = a} :: RegisterCACertificate)

-- | Metadata which can be used to manage the CA certificate.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
registerCACertificate_tags :: Lens.Lens' RegisterCACertificate (Prelude.Maybe [Tag])
registerCACertificate_tags = Lens.lens (\RegisterCACertificate' {tags} -> tags) (\s@RegisterCACertificate' {} a -> s {tags = a} :: RegisterCACertificate) Prelude.. Lens.mapping Lens.coerced

-- | The CA certificate.
registerCACertificate_caCertificate :: Lens.Lens' RegisterCACertificate Prelude.Text
registerCACertificate_caCertificate = Lens.lens (\RegisterCACertificate' {caCertificate} -> caCertificate) (\s@RegisterCACertificate' {} a -> s {caCertificate = a} :: RegisterCACertificate)

-- | The private key verification certificate.
registerCACertificate_verificationCertificate :: Lens.Lens' RegisterCACertificate Prelude.Text
registerCACertificate_verificationCertificate = Lens.lens (\RegisterCACertificate' {verificationCertificate} -> verificationCertificate) (\s@RegisterCACertificate' {} a -> s {verificationCertificate = a} :: RegisterCACertificate)

instance Core.AWSRequest RegisterCACertificate where
  type
    AWSResponse RegisterCACertificate =
      RegisterCACertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterCACertificateResponse'
            Prelude.<$> (x Core..?> "certificateArn")
            Prelude.<*> (x Core..?> "certificateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterCACertificate where
  hashWithSalt salt' RegisterCACertificate' {..} =
    salt'
      `Prelude.hashWithSalt` verificationCertificate
      `Prelude.hashWithSalt` caCertificate
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` registrationConfig
      `Prelude.hashWithSalt` allowAutoRegistration
      `Prelude.hashWithSalt` setAsActive

instance Prelude.NFData RegisterCACertificate where
  rnf RegisterCACertificate' {..} =
    Prelude.rnf setAsActive
      `Prelude.seq` Prelude.rnf verificationCertificate
      `Prelude.seq` Prelude.rnf caCertificate
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf registrationConfig
      `Prelude.seq` Prelude.rnf allowAutoRegistration

instance Core.ToHeaders RegisterCACertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON RegisterCACertificate where
  toJSON RegisterCACertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("registrationConfig" Core..=)
              Prelude.<$> registrationConfig,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("caCertificate" Core..= caCertificate),
            Prelude.Just
              ( "verificationCertificate"
                  Core..= verificationCertificate
              )
          ]
      )

instance Core.ToPath RegisterCACertificate where
  toPath = Prelude.const "/cacertificate"

instance Core.ToQuery RegisterCACertificate where
  toQuery RegisterCACertificate' {..} =
    Prelude.mconcat
      [ "setAsActive" Core.=: setAsActive,
        "allowAutoRegistration"
          Core.=: allowAutoRegistration
      ]

-- | The output from the RegisterCACertificateResponse operation.
--
-- /See:/ 'newRegisterCACertificateResponse' smart constructor.
data RegisterCACertificateResponse = RegisterCACertificateResponse'
  { -- | The CA certificate ARN.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The CA certificate identifier.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterCACertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'registerCACertificateResponse_certificateArn' - The CA certificate ARN.
--
-- 'certificateId', 'registerCACertificateResponse_certificateId' - The CA certificate identifier.
--
-- 'httpStatus', 'registerCACertificateResponse_httpStatus' - The response's http status code.
newRegisterCACertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterCACertificateResponse
newRegisterCACertificateResponse pHttpStatus_ =
  RegisterCACertificateResponse'
    { certificateArn =
        Prelude.Nothing,
      certificateId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The CA certificate ARN.
registerCACertificateResponse_certificateArn :: Lens.Lens' RegisterCACertificateResponse (Prelude.Maybe Prelude.Text)
registerCACertificateResponse_certificateArn = Lens.lens (\RegisterCACertificateResponse' {certificateArn} -> certificateArn) (\s@RegisterCACertificateResponse' {} a -> s {certificateArn = a} :: RegisterCACertificateResponse)

-- | The CA certificate identifier.
registerCACertificateResponse_certificateId :: Lens.Lens' RegisterCACertificateResponse (Prelude.Maybe Prelude.Text)
registerCACertificateResponse_certificateId = Lens.lens (\RegisterCACertificateResponse' {certificateId} -> certificateId) (\s@RegisterCACertificateResponse' {} a -> s {certificateId = a} :: RegisterCACertificateResponse)

-- | The response's http status code.
registerCACertificateResponse_httpStatus :: Lens.Lens' RegisterCACertificateResponse Prelude.Int
registerCACertificateResponse_httpStatus = Lens.lens (\RegisterCACertificateResponse' {httpStatus} -> httpStatus) (\s@RegisterCACertificateResponse' {} a -> s {httpStatus = a} :: RegisterCACertificateResponse)

instance Prelude.NFData RegisterCACertificateResponse where
  rnf RegisterCACertificateResponse' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf certificateId
