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
-- Module      : Amazonka.RDS.Types.Certificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.Certificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A CA certificate for an Amazon Web Services account.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The type of the certificate.
    certificateType :: Prelude.Maybe Prelude.Text,
    -- | Whether there is an override for the default certificate identifier.
    customerOverride :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | If there is an override for the default certificate identifier, when the
    -- override expires.
    customerOverrideValidTill :: Prelude.Maybe Core.ISO8601,
    -- | The final date that the certificate continues to be valid.
    validTill :: Prelude.Maybe Core.ISO8601,
    -- | The unique key that identifies a certificate.
    certificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The thumbprint of the certificate.
    thumbprint :: Prelude.Maybe Prelude.Text,
    -- | The starting date from which the certificate is valid.
    validFrom :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateType', 'certificate_certificateType' - The type of the certificate.
--
-- 'customerOverride', 'certificate_customerOverride' - Whether there is an override for the default certificate identifier.
--
-- 'certificateArn', 'certificate_certificateArn' - The Amazon Resource Name (ARN) for the certificate.
--
-- 'customerOverrideValidTill', 'certificate_customerOverrideValidTill' - If there is an override for the default certificate identifier, when the
-- override expires.
--
-- 'validTill', 'certificate_validTill' - The final date that the certificate continues to be valid.
--
-- 'certificateIdentifier', 'certificate_certificateIdentifier' - The unique key that identifies a certificate.
--
-- 'thumbprint', 'certificate_thumbprint' - The thumbprint of the certificate.
--
-- 'validFrom', 'certificate_validFrom' - The starting date from which the certificate is valid.
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { certificateType = Prelude.Nothing,
      customerOverride = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      customerOverrideValidTill = Prelude.Nothing,
      validTill = Prelude.Nothing,
      certificateIdentifier = Prelude.Nothing,
      thumbprint = Prelude.Nothing,
      validFrom = Prelude.Nothing
    }

-- | The type of the certificate.
certificate_certificateType :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateType = Lens.lens (\Certificate' {certificateType} -> certificateType) (\s@Certificate' {} a -> s {certificateType = a} :: Certificate)

-- | Whether there is an override for the default certificate identifier.
certificate_customerOverride :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Bool)
certificate_customerOverride = Lens.lens (\Certificate' {customerOverride} -> customerOverride) (\s@Certificate' {} a -> s {customerOverride = a} :: Certificate)

-- | The Amazon Resource Name (ARN) for the certificate.
certificate_certificateArn :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateArn = Lens.lens (\Certificate' {certificateArn} -> certificateArn) (\s@Certificate' {} a -> s {certificateArn = a} :: Certificate)

-- | If there is an override for the default certificate identifier, when the
-- override expires.
certificate_customerOverrideValidTill :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_customerOverrideValidTill = Lens.lens (\Certificate' {customerOverrideValidTill} -> customerOverrideValidTill) (\s@Certificate' {} a -> s {customerOverrideValidTill = a} :: Certificate) Prelude.. Lens.mapping Core._Time

-- | The final date that the certificate continues to be valid.
certificate_validTill :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_validTill = Lens.lens (\Certificate' {validTill} -> validTill) (\s@Certificate' {} a -> s {validTill = a} :: Certificate) Prelude.. Lens.mapping Core._Time

-- | The unique key that identifies a certificate.
certificate_certificateIdentifier :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateIdentifier = Lens.lens (\Certificate' {certificateIdentifier} -> certificateIdentifier) (\s@Certificate' {} a -> s {certificateIdentifier = a} :: Certificate)

-- | The thumbprint of the certificate.
certificate_thumbprint :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_thumbprint = Lens.lens (\Certificate' {thumbprint} -> thumbprint) (\s@Certificate' {} a -> s {thumbprint = a} :: Certificate)

-- | The starting date from which the certificate is valid.
certificate_validFrom :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_validFrom = Lens.lens (\Certificate' {validFrom} -> validFrom) (\s@Certificate' {} a -> s {validFrom = a} :: Certificate) Prelude.. Lens.mapping Core._Time

instance Core.FromXML Certificate where
  parseXML x =
    Certificate'
      Prelude.<$> (x Core..@? "CertificateType")
      Prelude.<*> (x Core..@? "CustomerOverride")
      Prelude.<*> (x Core..@? "CertificateArn")
      Prelude.<*> (x Core..@? "CustomerOverrideValidTill")
      Prelude.<*> (x Core..@? "ValidTill")
      Prelude.<*> (x Core..@? "CertificateIdentifier")
      Prelude.<*> (x Core..@? "Thumbprint")
      Prelude.<*> (x Core..@? "ValidFrom")

instance Prelude.Hashable Certificate where
  hashWithSalt salt' Certificate' {..} =
    salt' `Prelude.hashWithSalt` validFrom
      `Prelude.hashWithSalt` thumbprint
      `Prelude.hashWithSalt` certificateIdentifier
      `Prelude.hashWithSalt` validTill
      `Prelude.hashWithSalt` customerOverrideValidTill
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` customerOverride
      `Prelude.hashWithSalt` certificateType

instance Prelude.NFData Certificate where
  rnf Certificate' {..} =
    Prelude.rnf certificateType
      `Prelude.seq` Prelude.rnf validFrom
      `Prelude.seq` Prelude.rnf thumbprint
      `Prelude.seq` Prelude.rnf certificateIdentifier
      `Prelude.seq` Prelude.rnf validTill
      `Prelude.seq` Prelude.rnf customerOverrideValidTill
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf customerOverride
